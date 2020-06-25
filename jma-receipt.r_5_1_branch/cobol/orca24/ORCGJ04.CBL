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
       PROGRAM-ID.                 ORCGJ04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会 
      *  コンポーネント名  : 診療行為入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01/05/17  年齢計算をＪ０２へ
      * 02.00.00     MCC−多々納  01/05/22  入力コードをセットコードを表示等
      * 02.00.01     MCC-多々納   02/04/12  自費の修正
      * 02.00.02     NACL-多々納  02/07/29  パラメタファイルをＤＢへ
      * 02.00.03     NACL−多々納 02/09/19  インターフェースの変更
      * 02.00.04     NACL-多々納  02/11/20  自賠責器材追加
      * 02.00.05     NACL-多々納  02/12/02  食事の追加
      * 02.00.06     NACL-多々納  02/12/18  診療種別の判定追加
      * 02.00.07     NACL-多々納  05/04/20  診療会計附加情報追加
      * 02.00.08     NACL-多々納  05/11/30  MONFUNC 対応
      * 03.04.00     NACL-多々納  06/11/XX  剤追加他
      * 03.04.00     NACL-多々納  06/11/13  検査正式名称表示フラグ追加
      * 03.04.00     NACL-多々納  06/12/01  自賠責健保準拠追加
      * 03.04.01     NACL-多々納  06/12/14  薬剤附加チェック追加
      * 03.04.02     NACL-多々納  07/02/28  包括診療追加対応
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 03.05.01     NACL-多々納  07/07/18  皮下筋肉注射変換なし対応
      *                                     "0085XXXXX"コメント対応
      * 04.00.00     NACL-多々納  07/09/XX  テーブル４００対応
      * 04.01.00     NACL-多々納  07/10/XX  悪性腫瘍検査対応
      * 04.01.00     NACL-多々納  07/11/16  公務災害・公害対応
      * 04.01.00     NACL-多々納  07/12/XX  患者禁忌薬剤対応
      * 04.01.00     NACL-多々納  08/01/XX  チェックマスタ変更対応
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０年４月改正
      * 04.05.00     NACL-多々納  09/06/XX  数量小数５桁対応
      * 04.05.00     NACL-多々納  09/09/16  "0086XXXXX"コメント対応
      * 04.06.00     NACL-多々納  10/07/01  残量廃棄自動算定対応
      * 04.06.00     NACL-多々納  10/10/XX  包括算定（剤）対応
      * 04.08.00     NACL-多々納  13/06/XX  労災レセ電対応
      * 04.07.00     NACL-多々納  14/02/13  労災金額％加算対応
      * 04.08.00     NACL-多々納  14/12/XX  平成２７年１月改正対応
      * 04.08.00     NACL-多々納  16/05/XX  一般名表示対応
      *  04.08.00    NACL-多々納  16/08/XX  持参薬対応
      *  04.08.00    NACL-多々納  16/11/XX  投薬投与量チェック区分対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年０４月改正対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      ***************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(1500).
      *
      *
       WORKING-STORAGE             SECTION.
      * ＳＰＡパラメタデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "J04SPASCR".
      *    03  FILE-HOSPNUM        PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J04SCR-SPA".
      *
      *  診療行為テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *    03  STS-SRYKBN          PIC X(02).
      *    03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *
           03  FLG-GMN             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SST             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-YAKZAI          PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-ARI             PIC 9(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SET-HEN         PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    漢字短縮
           03  FLG-TANSYUKU        PIC 9(01).
      *
           03  FLG-MAE             PIC 9(01).
      *    自費入力
           03  FLG-JIHI           PIC 9(01).
      *
           03  FLG-OK             PIC 9(01).
           03  FLG-OK2            PIC 9(01).
           03  FLG-UPD            PIC 9(01).
           03  FLG-UPD2           PIC 9(01).
           03  FLG-ERR            PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *    空白入力
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-CHK-OK          PIC 9(01).
      *
           03  FLG-FIRUMU          PIC 9(01).
           03  FLG-SYUNOU-ERR      PIC 9(01).
      *
           03  FLG-NAIFUTAN        PIC 9(01).
           03  FLG-NAIBUN          PIC 9(01).
           03  FLG-GEN             PIC 9(01).
      *
           03  FLG-CHK-RSI         PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-HKNHEN          PIC 9(01).
      *
           03  FLG-SRYKBN-HEN      PIC 9(01).
           03  FLG-IN1             PIC 9(01).
           03  FLG-HENKOU          PIC 9(01).
      *
           03  FLG-SIP             PIC 9(01).
      *H30.4
           03  FLG-NINPUKBN-IN     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDS                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-SIN             PIC 9(04).
           03  IDX-ERR             PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-YAKUSOKU        PIC 9(04).
           03  IDX-YAK             PIC 9(04).
           03  IDX-1               PIC 9(04).
           03  IDX-SET             PIC 9(04).
           03  IDX-D2              PIC 9(02).
           03  IDX-D1              PIC 9(02).
      *
           03  IDX-ATO             PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
      *
           03  IDX-J98             PIC 9(04).
      *
           03  IDH                 PIC 9(04).
      *
           03  IDX-SR              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MATUYMD         PIC X(08).
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
      *
           03  WRK-MOJISU          PIC 9(04).
           03  WRK-FREE            PIC 9(04).
      *
           03  WRK-KEI             PIC 9(08).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(02).
           03  WRK-STR             PIC 9(04).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZZZ.
               05  WRK-SURYO-Z19   REDEFINES  WRK-SURYO-Z1
                                   PIC ZZZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-KEICOM-X.
               05  WRK-KEICOM-Z    PIC ZZZZZZZ9.
           03  WRK-GOKEI-X.
               05  WRK-GOKEI-Z     PIC ZZZZZZZZ.
           03  WRK-NOZ             PIC ZZ9.
      *ver.4.7
           03  WRK-TENSU-GX.
               05  WRK-TENSU-G1    PIC X(01).
      *H31.4   05  WRK-TENSU-GZ    PIC ZZZZZZ.
               05  WRK-TENSU-GZ    PIC ZZZZZZZ.
               05  WRK-TENSU-G2    PIC X(01).
      *
           03  WRK-SURYO-H         PIC X(09).
           03  WRK-KAISU-H         PIC X(04).
           03  WRK-BUNGSU-H        PIC X(09).
           03  WRK-KAKERU          PIC X(01).
      *
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-TENSU           PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *    分画数フラグ
           03  FLG-BUIKBN          PIC X(01).
      *
           03  WRK-KINGAK          PIC 9(08).
           03  WRK-KAISUKEI        PIC 9(08).
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-BUNGSU          PIC 9(08).
           03  WRK-GOKEI           PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYKBN-1        PIC X(02).
      *
           03  WRK-SURYOKEI        PIC 9(07)V9(05).
           03  WRK-SRYCDKEI        PIC 9(14).
           03  WRK-MEISAISU        PIC 9(07).
           03  WRK-ZAIKAISU        PIC 9(03).
           03  WRK-GOKEI-9         PIC 9(10).
           03  WRK-SRYCD-9         PIC 9(09).
      *
           03  WRK-EN              PIC 9(08).
           03  WRK-TANKA           PIC 9(08).
      *
           03  WRK-TANINAME        PIC X(04).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *
           03  WRK-PARAEDABAN      PIC 9(04).
      *D   03  WRK-ZAIIDX          PIC 9(04).
           03  WRK-H-SRYSYUKBN     PIC X(03).
           03  WRK-H-SRYKBN        PIC X(02).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-JIHIMONEY           PIC  9(08).
               05  WRK-ZAIKBN              PIC  9(03).
      *     一部負担金計算用
           03  WRK-S01-TENSU       PIC 9(08).
      *
           03  WRK-NAICHORYO       PIC 9(07).
           03  WRK-GAICHORYO       PIC 9(07).
           03  WRK-SYOHORYO        PIC 9(07).
      *
           03  WRK-MOJI            PIC X(01).
           03  WRK-ATAI            PIC X(08).
      *   注射・点滴用チェック
           03  FLG-MADOKU          PIC X(01).
           03  FLG-SEIBETU         PIC X(01).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-CHUSYA          PIC 9(09).
           03  IDX-S               PIC 9(04).
           03  IDX-SW              PIC 9(04).
      *    警告済フラグ保存
           03  WRK-KNKFLG          PIC X(01).
           03  WRK-KNKUMU          PIC X(01).
           03  WRK-KEIFLG          PIC X(01).
           03  WRK-KZMFLG          PIC X(01).
           03  WRK-SDOFLG          PIC X(01).
           03  WRK-CHKFLG          PIC X(01).
           03  WRK-TIMEFLG         PIC X(01).
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
      *
           03  WRK-KINKIFLG            PIC X(01).
           03  WRK-PTKINKI-ARI         PIC 9(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *Ｋ９８、Ｋ９９戻りＩＤＸ保存
           03  WRK-HZN-IDX         PIC 9(04).
      *
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-MAX-IDX         PIC 9(04).
      *
      *    自費用
           03  WRK-TENTTLKBN       PIC 9(03).
           03  WRK-HOZ-KEI         PIC 9(08).
      *
      *    約束セット用
           03  WRK-IDX-YAKUSOKU.
               05  WRK-IDX-YAK         PIC 9(04)    OCCURS  20.
      *    入力位置判定用
           03  CNT-IN              PIC 9(02).
           03  WRK-IN-SURYO        PIC 9(02).
           03  WRK-IN-IDX          PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-DAY                 PIC 9(03).
      *    入力時表示用
           03  WRK-ERR-IDX             PIC 9(04).
           03  IDX-INCNT               PIC 9(04).
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(54).
           03  FLG-IN-ATAI             PIC 9(01).
      *    検査回数
           03  WRK-KENSA-CNT       PIC 9(04).
           03  WRK-KENSA-MAX       PIC 9(04).
           03  WRK-CHK-MAX         PIC 9(04).
           03  IDX-KEN             PIC 9(04).
           03  IDX-KEN2            PIC 9(04).
           03  FLG-KENSA           PIC 9(01).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
      *
           03  WRK-JIDMSG              PIC X(80).
      *
           03  WRK-ERRCD           PIC X(04).
      *
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
               05  WRK-HZN-ATAI5           PIC X(10).
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
      *R02.4   05  WRK-MEISYO              PIC X(78).
               05  WRK-MEISYO              PIC X(138).
           03  WRK-MEISYO-GHZN.
               05  WRK-MEIHHZN             PIC X(02).
      *R02.4   05  WRK-MEISYOHZN           PIC X(78).
               05  WRK-MEISYOHZN           PIC X(138).
           03  IDX-MEI                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    診療種別
           03  WRK-MAE-SRYSYUKBN          PIC X(03).
      *
      *    労災施術加算用
           03  WRK-ENTANKA             PIC S9(08).
           03  IDR-1                   PIC 9(04).
           03  IDR-2                   PIC 9(04).
           03  WRK-TENKEISAN           PIC 9(08)V99.
           03  WRK-TENSUKEI-9          PIC S9(08).
           03  WRK-TENSUKEI-G          PIC S9(08).
           03  WRK-TENSUKEI-K          PIC S9(08).
      *
           03  WRK-MAX                 PIC 9(04).
      *
      *    行位置編集
       01  WRK-GMNGYO-AREA.
           03  WRK-MAP-MAX         PIC 9(04).
           03  WRK-MAP-CUR         PIC 9(04).
           03  WRK-MAP-CUR2        PIC 9(04).
           03  WRK-MAP-CUR3        PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
           03  WRK-MAP-PAGE        PIC 9(04).
           03  WRK-MAP-PAGE2       PIC 9(04).
           03  WRK-MAP-PAGE3       PIC 9(04).
           03  WRK-MAP-MAXPAGE     PIC 9(04).
      *
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO        PIC X(11).
               05  WRK-H-TANINAME     PIC X(04).
      *H31.4   05  WRK-H-F1           PIC X(02).
               05  WRK-H-F1           PIC X(01).
      *H31.4   05  WRK-H-ZAIKEI       PIC X(08).
               05  WRK-H-ZAIKEI-G.
                   07  WRK-H-ZAIKEI-0     PIC X(01).
                   07  WRK-H-ZAIKEI       PIC X(08).
      *
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC Z(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-ZZ              PIC ZZZ.
      *
      *    保険変更用
       01  WRK-HKNCOMBI-HEN-AREA.
           03  TBL-ZAINUM-AREA.
               05  TBL-ZAINUM-G        OCCURS   200.
                   07  TBL-ZAINUM          PIC 9(08).
                   07  TBL-SRYKBN          PIC X(02).
           03  WRK-DD                  PIC 9(02).
           03  WRK-DOUJI-RENNUM        PIC 9(01).
           03  WRK-KEY-DOUJI-RENNUM    PIC 9(01).
           03  IDX-J                   PIC 9(04).
           03  IDX-J2                  PIC 9(04).
           03  IDX-J3                  PIC 9(04).
           03  IDX-J4                  PIC 9(04).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
           03  FLG-JKNKSN          PIC 9(02).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-SUR-MCNT        PIC 9(02).
           03  WRK-MCNT1           PIC 9(02).
           03  WRK-MCNT2           PIC 9(02).
           03  WRK-MCNT3           PIC 9(02).
           03  WRK-MCNT4           PIC 9(02).
           03  WRK-MCNT5           PIC 9(02).
           03  WRK-CD              PIC X(54).
           03  WRK-KAI             PIC X(20).
           03  WRK-SUR             PIC X(20).
           03  WRK-MOJI1           PIC X(10).
           03  WRK-MOJI2           PIC X(10).
           03  WRK-MOJI3           PIC X(10).
           03  WRK-MOJI4           PIC X(10).
           03  WRK-MOJI5           PIC X(10).
           03  IDM                 PIC 9(04).
           03  IDMS                PIC 9(04).
      *
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDX-D               PIC 9(02).
      *
      *    検査　判断グループ
       01  WRK-KENSA-AREA.
           03  WRK-KENSA-TBL                       OCCURS   20.
               05  WRK-KENSAGRP        PIC 9(02).
               05  WRK-KEN-SRYCD       PIC X(09).
               05  WRK-KEN-TENSU       PIC 9(05)V9(03).
           03  WRK-KENIDX              PIC 9(04).
      *
      *    禁忌チェック用テーブル
       01  WRK-KNKCHK-AREA.
           03  WRK-KNK-TBL             OCCURS   30.
               05  WRK-KNK-TOUMEI          PIC X(40). 
               05  WRK-KNK-TOUSRYCD        PIC X(09). 
               05  WRK-KNK-TBL2A.
                   07  WRK-KNK-TBL2            OCCURS   30.
                       09  WRK-KNK-KNKMEI          PIC X(40).
                       09  WRK-KNK-SRYYM           PIC X(06).
                       09  WRK-KNK-SRYYMW          PIC X(07).
                       09  WRK-KNK-SRYCD           PIC X(09).
           03  WRK-KNKMEI-G.
               05  WRK-KNKMEI-T            OCCURS   50.
                   07  WRK-KNKMEI          PIC X(38).
                   07  WRK-KNKCUR          PIC X(02).
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   07  WRK-SRT-KNKMEI          PIC X(40).
                   07  WRK-SRT-SRYYM           PIC X(06).
                   07  WRK-SRT-SRYYMW          PIC X(07).
                   07  WRK-SRT-SRYCD           PIC X(09).
           03  WRK-KEY-SRYYM               PIC X(06).
           03  WRK-KEY-SRYCD               PIC X(09).
      *
      *    コメントの入力値用
       01  WRK-INPUT-SET-AREA.
           03  WRK-INPUT-TBL.
               05  WRK-INPUT-SET       PIC 9(03)   OCCURS  8.
           03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
               05  WRK-INPUT-TBL2          OCCURS  4.
                   07  WRK-INPUT-ICHI      PIC 9(03).
                   07  WRK-INPUT-KETA      PIC 9(03).
           03  WRK-KETA                PIC 9(02).
           03  WRK-KETA2               PIC 9(02).
           03  WRK-ICHI                PIC 9(02).
           03  WRK-ICHI2               PIC 9(02).
           03  IDX-KNJ                 PIC 9(02).
           03  IDX-KNJ2                PIC 9(04).
           03  WRK-ZZZ9-X.
               05  WRK-ZZZ9                PIC ZZZZ9.
           03  WRK-KANJI.
               05  WRK-KANJI-X             PIC X(02)
                                           OCCURS   5.
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *画面最大行
       01  MAX-GMN                 PIC 9(04)   VALUE   20.
      *画面ページ数（処理最大行÷画面最大行）
       01  MAX-PAGE                PIC 9(04)   VALUE   20.
      *
      *H30.4
      *    時間外加算テーブル(H30.4分）（妊婦加算含）
           COPY     "CMTIMEKSN2018.INC".
      *
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
       01  WRK-HOZ-IDX             PIC 9(04).
      *
      *H26.12
      *    スパ領域ワーク
           COPY    "COMMON-SPA"    REPLACING
                                       //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1008.INC".
      *
           COPY    "CPSK1038.INC".
      *
      *    レセプト印刷情報
           COPY  "CPSK200501.INC".
      *
      *    点数算定基準
           COPY    "CPSK1201.INC".
      *
           COPY    "CPSK1030.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS   10.
           COPY    "CPSRYACT.INC"    REPLACING
                                     //SRY-// BY //TBL-SRY-//.
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計マスタ−
       01  HZN-SRYACCT-REC.
           COPY    "CPSRYACCT.INC"    REPLACING
                                     //ACCT-// BY //HZN-ACCT-//.
      *
       01  HZN2-SRYACCT-REC.
           COPY    "CPSRYACCT.INC"    REPLACING
                                     //ACCT-// BY //HZN2-ACCT-//.
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    受診履歴ワーク
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *
      *    受診履歴ワーク２
       01  HZN2-JYURRK-REC.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //HZN2-JYURRK-//.
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    附加情報
       01  WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      ***  COPY    "CPORCSC91.INC".
      *
      *    点数編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    エラーメッセージパラメタ
           COPY    "CPORCSC96.INC".
      *
      *    入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    投与量等チェックパラメタ
           COPY    "CPORCSC98.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    診療会計剤チェックパラメタ
           COPY    "CPORCSCZAICHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    保険算定チェックサブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *H28.3
      *    院外処方名編集パラメタ
           COPY    "CPORCSCGENNAME.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA24SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      **** INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J04
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    SPACE           TO  SPA-ERRMSG2
           MOVE    ZERO            TO  FLG-END
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-J04SPA-SET-SEC
      *    画面制御
           MOVE    SPACE           TO  WRK-PUTTYPE
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-J04SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J04         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
                                       OR  "JERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               IF      SPA-KEI-FLG         =   1
      *            終了
                   MOVE    "JOIN"          TO  WRK-PUTTYPE
                   PERFORM 410021-END-SYORI-SEC
               ELSE
                   PERFORM 5001-MAPCUR-SEC
               END-IF
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "J04"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡパラメタデータ読込処理
      *****************************************************************
       1002-J04SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF     SPA-HOSPNUM  NOT NUMERIC
               GO   TO  1002-J04SPA-SET-EXT
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC        TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    行位置とカーソル位置の決定
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   MAX-GMN
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           .
       1002-J04SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡパラメタデータＷＲＩＴＥ処理
      *****************************************************************
       1003-J04SPA-OUT-SEC             SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(IDX)   =   SPACE )
               AND     (IDX            >   SPA-GMN-MAX  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-TBL (IDX)   TO  PARA-DATA-REC
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J04 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       1003-J04SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-GMN
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *********元画面表示
      *        IF      SPA-MOTOPG(1:3)     =   "Y01"  OR  "U01" 
      *            MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
      *            MOVE    LNK-SCRSPA      TO  SPA-J04
      *            MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *       END-IF
      *       予約とエラー表示の時、そのまま画面表示へ
      *        IF      SPA-MOTOPG(1:3)     =   "Y01"  OR
      *                                        "U01"
      *            GO      TO      300-SCREEN-EXT
      ******** END-IF
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "J99"
                   IF      SPA-J99-DATA    NOT =   SPACE
                       MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
                                                   TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                       MOVE    SPA-J99-DATA        TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
      *
      *                入力コード・名称チェック処理
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *                画面ＳＰＡチェック処理
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "J98"
      *@
      *            悪性腫瘍の検査
                   IF     SPA-AKUSEI-FLG       =   1
                       PERFORM 30112-AKUSEI-COMMENT-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *H30.9
      *            選択式コメント
                   IF      SPA-SELCOM-FLG      =   1
                       PERFORM 30113-SELCOMMNT-COMMENT-SEC
                   ELSE
      *
      *
                   IF      SPA-J98-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-J98-SET-SEC
                       MOVE    1               TO  FLG-GMN
                   ELSE
                       MOVE    "1"             TO  SPA-COM-CUR
                                                      (SPA-GMN-IDX)
                       MOVE    2               TO  FLG-GMN
                   END-IF
      *
                   END-IF
      *
                   END-IF
               WHEN    "J041"
      *             禁忌一覧 ＯＫの時、更新処理へ
                   IF      SPA-J041-CHK        =   1
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
                   END-IF
                   MOVE    2               TO  FLG-GMN
               WHEN    "J042"
      *            禁忌画面より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "JID1"
      *            確認画面より
                   PERFORM 3003-JID1-SET-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
      *H26.12
           IF      FLG-GMN         NOT =   ZERO
               IF      SPA-ERRCD       NOT =   SPACE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
      *            エラー編集
      *            数量ゼロの場合、エラー表示しない
                   IF      SPA-ERRCD           =   "A001"
                       MOVE    SPACE               TO  SPA-ERRCD
                   ELSE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     初期画面表示処理
      *****************************************************************
       3000-SCREEN-INIT-SEC                  SECTION.
      *
           MOVE    SPACE           TO  SPA-JIDCD
           MOVE    SPACE           TO  SPA-JID1-FLG
      *
           INITIALIZE              SPA-J04
           INITIALIZE              SPA-SCR-REC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           IF      SPA-GMN-HKNCOMBI    =   SPACE
               MOVE    10              TO  SPA-GMN-CUR
           END-IF
      *
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
               WHEN    "0103"
      *            戻るの確認
                   IF      SPA-JID1-FLG        =   "OK"
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
      *
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                       END-IF
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-JIDCD
                   MOVE    SPACE           TO  SPA-JID1-FLG
      *
               WHEN    "0102"
      *            外用の確認
                   MOVE    SPACE           TO  SPA-JIDCD
                   IF      SPA-JID1-FLG        =   "OK"
      *                更新処理
                       PERFORM 4100-TOROKU-SEC
      *
                       IF      SPA-JIDCD       NOT =   SPACE
                           PERFORM 520-JIDSET-SEC
                       END-IF
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-JID1-FLG
      *
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    システム管理設定
           PERFORM 3101-SYSKANRI-SET-SEC
      *
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *
           MOVE    "2"             TO  FLG-TOROKU
      *
           IF      SPA-J04-SYORI       =   1
      *        新規
               MOVE    ZERO                TO  SPA-GMN-MAX
               MOVE    ZERO                TO  IDX2
               MOVE    ZERO                TO  SPA-CHOKITOFLG
           ELSE
      *        診療行為マスターを編集する
               PERFORM 3103-SINKOU-HEN-SEC
           END-IF
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SRYKA-MAX )
               IF      SPA-J01-SRYKA       =   SPA-GMN-SRYKAL(IDX)
                   MOVE    SPA-GMN-SRYKAMEIL(IDX)  TO   SPA-J01-SRYKAMEI
                   MOVE    SPA-SRYKA-MAX       TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-SRYKBN          TO  SPA-J04-SRYKBN
      *
           MOVE    SPA-JIHOKBN         TO  SPA-J04-JIHOKBN
      *    包括診療分
           MOVE    "0 　　　　"        TO  SPA-GMN-HKTKBN-LST (1)
           MOVE    "1 包括診療"        TO  SPA-GMN-HKTKBN-LST (2)
           MOVE    2                   TO  SPA-HKTKBN-MAX
           IF      SPA-J04-JIHOKBN     =   "1"
               MOVE    SPA-GMN-HKTKBN-LST (2)  TO  SPA-GMN-HKTKBN-G
           ELSE
               MOVE    SPA-GMN-HKTKBN-LST (1)  TO  SPA-GMN-HKTKBN-G
           END-IF
      *
      *    保険組合せ
           MOVE    SPA-HKNCOMBINUM     TO  SPA-J04-HKNCOMBI
           MOVE    SPA-HKNKBN          TO  SPA-J04-HKNKBN
           MOVE    SPA-RSI-HKNKBN      TO  SPA-J04-RSI-HKNKBN
           MOVE    SPA-RSI-JUNKYO      TO  SPA-J04-RSI-JUNKYO
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBI
           IF     (SPA-NYUGAIKBN       =   1   )
               PERFORM 31031-HKNCOMBI-HEN-SEC
           ELSE
               INITIALIZE  SPA-HKNCOMBI-AREA
               MOVE    SPA-J02-HKNCOMBIMEI TO  SPA-GMN-HKNCOMBI-MEI
           END-IF
      *    入力コードの画面用再編集
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD(IDX-1)  =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-1)  =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD(IDX-1)(1:1)
                   WHEN    "."
                       CONTINUE
      *            約束セット名称編集
                   WHEN    "S"
                       INITIALIZE                  WRK-MOJI-AREA
                       MOVE    IDX-1               TO  IDX
      *                入力コードの画面用再編集
                       PERFORM 3101-INPUTCD-HEN-SEC
                       MOVE    IDX-1               TO  SPA-GMN-IDX
      *                約束名称編集
                       MOVE    6                   TO  WRK-CD-MCNT
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                                   TO  WRK-INPUTCD
                       PERFORM 41011921-INPSET-MEI-SEC
                       MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                        (SPA-GMN-IDX)
      *            その他
                   WHEN    OTHER
      *                入力コードの画面用再編集
                       INITIALIZE                  WRK-MOJI-AREA
                       MOVE    IDX-1               TO  IDX
                       PERFORM 3101-INPUTCD-HEN-SEC
                       MOVE    IDX-1               TO  SPA-GMN-IDX
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                                   TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG (IDX)
               MOVE    "1"                 TO  SPA-COM-HKEIFLG(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG2(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG3(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG4(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG5(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG6(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG1(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG(IDX)
           END-PERFORM
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
      *    剤点数不一致
      *    訂正時点数変更チェック
           MOVE    ZERO                TO  SPA-TENKAI-CHK
           MOVE    ZERO                TO  SPA-TENKAI-ERR
           MOVE    ZERO                TO  FLG-SIP
      *    点数変更エラー
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
      *H28.4
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "6" )
                  AND (SPA-COM-SRYKBN (IDX)        =   "23")
                  AND (SPA-COM-SIPPUKBN(IDX)       =   "1" )
                   MOVE    1                   TO  FLG-SIP
               END-IF
      *H25.12
               IF     (SPA-COM-SRYKBN(IDX)     =   "95"
                                               OR  "96" ) AND
                      (SPA-COM-JIHIMONEY(IDX)  >   ZERO )
                   ADD     SPA-COM-JIHIMONEY(IDX)
                                                   TO  WRK-TENSU
               END-IF
               IF     (SPA-COM-ZAIEND(IDX)     =   "1" )
                   IF      SPA-COM-SRYKBN(IDX)     =   "95"
                                                   OR  "96"
                       CONTINUE
                   ELSE
                       MOVE    SPA-COM-TENSU (IDX)
                                                   TO  WRK-TENSU
                   END-IF
                   IF      WRK-TENSU       NOT =   SPA-ZAITEN
                       MOVE    1               TO  SPA-TENKAI-ERR
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               END-IF
               IF     (SPA-COM-KZMFLG (IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG2(IDX)    NOT =   SPACE )
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD (IDX)
               END-IF
               MOVE    SPACE           TO  SPA-COM-KZMFLG (IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG2(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG4(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG5(IDX)
               MOVE    SPACE           TO  SPA-COM-KINKIFLG(IDX)
           END-PERFORM
      *
           IF      SPA-TENKAI-ERR      =   1
               MOVE    "K103"              TO  SPA-ERRCD
               IF      FLG-SIP             =   1
                   MOVE    "K104"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-TENKAI-ERR
               END-IF
           END-IF
      *
           MOVE    SPA-ZAINUM          TO  SPA-MAE-ZAINUM
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理設定編集処理
      *****************************************************************
       3101-SYSKANRI-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
      **** MOVE    "1"                 TO  SYS-1006-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI        =   1
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-KBNCD      =   "1"
      *            小児科外来診療科
                   IF      SYS-1006-SSTKIJUN (121) =   1
                       MOVE    1               TO  SPA-SYONIKA
                   ELSE
                       MOVE    0               TO  SPA-SYONIKA
                   END-IF
      *            時間外特例医療機関
                   IF      SYS-1006-SSTKIJUN (011) =   1
                       MOVE    1               TO  SPA-JIKANTOKU
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    時間外特例医療機関の施設基準はすべてありとする
           MOVE    1                   TO  SPA-JIKANTOKU
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1008-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1008-REC
      *
      *        時間外加算特例乳幼児
               IF      SYS-1008-JIKANTOKU-NYU-FLG  =   SPACE
                   MOVE    ZERO            TO  SPA-JIKANTOKU-NYU
               ELSE
                   MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                           TO  SPA-JIKANTOKU-NYU
      *H30.4
      *            H30.4から産婦人科特例対応
                   IF     (SPA-SRYYMD          <   "20180401" )
                       IF      SPA-JIKANTOKU-NYU   =   "2"
                           MOVE    "1"             TO  SPA-JIKANTOKU-NYU
                       END-IF
                       IF      SPA-JIKANTOKU-NYU   =   "3"
                           MOVE    ZERO            TO  SPA-JIKANTOKU-NYU
                       END-IF
                   END-IF
               END-IF
      *        数量ゼロ入力
               IF      SYS-1008-SURYOZERO-FLG  =   SPACE
                   MOVE    ZERO            TO  SPA-SURYOZERO-FLG
               ELSE
                   MOVE    SYS-1008-SURYOZERO-FLG
                                           TO  SPA-SURYOZERO-FLG
               END-IF
      *H28.11
      *        薬剤投与量チェック
               IF      SYS-1008-TOUYOCHK-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-TOUYOCHK-FLG
               ELSE
                   MOVE    SYS-1008-TOUYOCHK-FLG
                                           TO  SPA-TOUYOCHK-FLG
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-JIKANTOKU-NYU
               MOVE    ZERO                TO  SPA-SURYOZERO-FLG
               MOVE    "0"                 TO  SPA-TOUYOCHK-FLG
           END-IF
      *
      *    システム管理検索（診療行為一括入金管理情報）
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1038-REC
      *        皮下筋肉注射料変換選択
               MOVE    SYS-1038-CHG310FLG      TO  SPA-CHG310FLG
      *        腫瘍マーカ一覧表示
               MOVE    SYS-1038-AKUSEIHYOFLG   TO  SPA-AKUSEIHYOFLG
      *        残量廃棄区分
               MOVE    SYS-1038-HAIKIKBN       TO  SPA-HAIKIKBN-FLG
           END-IF
           IF      SPA-CHG310FLG       =   SPACE
               MOVE    "0"                 TO  SPA-CHG310FLG
           END-IF
           IF      SPA-AKUSEIHYOFLG    =   SPACE
               MOVE    "0"                 TO  SPA-AKUSEIHYOFLG
           END-IF
      *    残量廃棄区分
           IF      SPA-HAIKIKBN-FLG    =   SPACE
               MOVE    "1"                 TO  SPA-HAIKIKBN-FLG
           END-IF
      *    Ｈ２２年４月から反映
           IF      SPA-SRYYMD          <   "20100401"
               MOVE    "1"                 TO  SPA-HAIKIKBN-FLG
           END-IF
      *ver.4.8
      *    労災レセ電対応
           IF      SPA-SRYYMD          >=  "20130801"
               MOVE    SPACE               TO  SYS-200501-REC
               INITIALIZE                      SYS-200501-REC
               MOVE    "2005"              TO  SYS-200501-KANRICD
               MOVE    "01"                TO  SYS-200501-KBNCD
               MOVE    SPA-SRYYMD          TO  SYS-200501-STYUKYMD
               MOVE    SPA-SRYYMD          TO  SYS-200501-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-200501-HOSPNUM
               MOVE    SYS-200501-REC      TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-200501-REC
                   IF      SYS-200501-ROURECEKBN   =   SPACE
                       MOVE    "0"             TO  SYS-200501-ROURECEKBN
                   END-IF
                   MOVE    SYS-200501-ROURECEKBN   TO  SPA-ROURECEKBN
               END-IF
           ELSE
               MOVE    ZERO                    TO  SPA-ROURECEKBN
           END-IF
      *
      *H27.1
      *    ステム管理共通（妥結率）
           PERFORM 1003-SYSKANRI-DAKETU-SEC
      *H28.2
      *    後発医薬品への変更可署名
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1030-REC
               MOVE    SYS-1030-KOUHATUKA  TO  SPA-KOUHATUKA
           ELSE
               MOVE    "0"                 TO  SPA-KOUHATUKA
           END-IF
      *
      *
           .
       3101-SYSKANRI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ステム管理（妥結率）設定処理
      *****************************************************************
       1003-SYSKANRI-DAKETU-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-DAKT50-KBN
      *
      *    病院・許可病床数２００以上
           IF     (SPA-HOSPSBT         =   1  )
              AND (SPA-BEDSU           >=  200)
              AND (SPA-SRYYMD          >=  "20150101")
      *
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "1201"              TO  SYS-KANRICD
               MOVE    "A0000001"          TO  SYS-KBNCD
               MOVE    SPA-SRYYMD          TO  SYS-STYUKYMD
               MOVE    SPA-SRYYMD          TO  SYS-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
      *
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1201-REC
      *            妥結率以下
                   IF      SYS-1201-DAKETUKBN  =   "1"
                       MOVE    1                   TO  SPA-DAKT50-KBN
                   END-IF
               END-IF
           END-IF
      *
           .
       1003-SYSKANRI-DAKETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ一覧編集処理
      *****************************************************************
       31031-HKNCOMBI-HEN-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBI-LISTG
           MOVE    SPA-J02-SRYYMD(1:6) TO  SPA-SRYYMD(1:6)
           MOVE    "00"                TO  SPA-SRYYMD(7:2)
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
      *
           MOVE    ZERO                TO  IDH
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )  OR
                              (IDH     >=  40              )
               ADD     1                   TO  IDH
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDH)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDH)
      *
               MOVE    ORCSHKN-NAI-HKNID (IDX)
                                       TO  SPA-NAI-THKNID  (IDH)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-THKNNUM  (IDH)
               MOVE    ORCSHKN-NAI-HKNKBN (IDX)
                                       TO  SPA-NAI-THKNKBN  (IDH)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH1HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH2HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH3HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH4HKNNUM (IDH)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDH)
      *        四肢区分
               MOVE    ORCSHKN-NAI-RSI-SISIKBN (IDX)
                                       TO  SPA-NAI-TRSI-SISIKBN (IDH)
      *        傷病年月日
               MOVE    ORCSHKN-NAI-RSI-SHOBYOYMD (IDX)
                                       TO  SPA-NAI-TRSI-SHOBYOYMD(IDH)
      *        労災区分
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-NAI-TRSI-HKNKBN  (IDH)
      *        適用期間
               MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-TTEKSTYMD  (IDH)
               MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-TTEKEDYMD  (IDH)
      *
               MOVE    IDH             TO  SPA-HKN-MAX
      *
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                       =   SPA-GMN-HKNCOMBI
                   MOVE    SPA-GMN-THKNCOMBINUM-G (IDH)
                                       TO  SPA-GMN-HKNCOMBI-G
               END-IF
           END-PERFORM
           MOVE    SPA-J02-SRYYMD      TO  SPA-SRYYMD
      *
           .
       31031-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-J98-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-J98 FROM    1   BY  1
                   UNTIL      (IDX-J98             >   10   )  OR
                              (SPA-J98-SRYCD(IDX-J98)  =   SPACE )
      *
               MOVE    "1"                 TO  SPA-COM-IN (SPA-GMN-IDX)
      *
               IF      IDX-J98             =   1
                   MOVE    SPA-J98-SRYCD(IDX-J98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   MOVE    SPA-J98-SRYCD(IDX-J98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               END-IF
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-J98-SET-EXT.
           EXIT.
      *
      *@@
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       5101-AKUSEI-J98-SEC       SECTION.
      *
           MOVE    SPA-AKUSEI-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-J98-AREA
           MOVE    "//*D009S"          TO  SPA-J98-INPUTCD
           MOVE    "A"                 TO  SPA-J98-CDSYUBETU
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-AKUSEI-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-J98-SYORI-SEC
           .
       5101-AKUSEI-J98-EXT.
           EXIT.
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       30112-AKUSEI-COMMENT-SEC       SECTION.
      *
           ADD     1                   TO  SPA-GMN-IDX 
           MOVE    SPA-GMN-IDX         TO  IDX-S
           IF      SPA-J98-SRYCD-G   NOT =   SPACE
               PERFORM 3011-J98-SET-SEC
           ELSE
      *    コメントコード追加
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "830000015"     TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
      *           入力コード・名称チェック処理
                  PERFORM 410112-KOUI-SPACHK-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-AKUSEI-FLG
           MOVE    ZERO                TO  SPA-AKUSEI-IDX
      *
           .
       30112-AKUSEI-COMMENT-EXT.
           EXIT.
      *H30.9
      *****************************************************************
      *    選択式コメントへ処理
      *****************************************************************
       30113-SELCOMMNT-COMMENT-SEC       SECTION.
      *
           IF     (SPA-SELCOM-IDX      >   ZERO  )
             AND  (SPA-COM-INPUTCD(SPA-SELCOM-IDX)(1:1)  =   "1"  )
             AND  (SPA-COM-RECEKISAI (SPA-SELCOM-IDX)    =   "1"
                                                         OR  "2"  )
      *        選択済み
               MOVE    "1"             TO  SPA-COM-RECEAUTO
                                                    (SPA-SELCOM-IDX)
           END-IF 
      *
      *    対象行の下に挿入
           IF      SPA-COM-INPUTCD (SPA-GMN-IDX) NOT =  SPACE
               ADD     1                   TO  SPA-GMN-IDX
               IF      SPA-GMN-IDX         <=  SPA-GMN-MAX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-SELCOM-FLG
           MOVE    ZERO                TO  SPA-SELCOM-IDX
      *
           PERFORM 3011-J98-SET-SEC
           MOVE    1                   TO  FLG-GMN
           .
       30113-SELCOMMNT-COMMENT-EXT.
           EXIT.
      *
      *@@
      *****************************************************************
      *   診療行為マスタより編集処理
      *****************************************************************
       3103-SINKOU-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  FLG-YAKZAI
           MOVE    ZERO                TO  FLG-FIRUMU
           MOVE    ZERO                TO  FLG-CHK-RSI
           MOVE    ZERO                TO  SPA-CHOKITOFLG
      *
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU
      *
           PERFORM
               UNTIL    (FLG-SRYACT        =   1          )
      *
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS             >   5    )  OR
                                  (SRY-SRYCD (IDS) =   SPACE)
                   PERFORM 31011-SPA-HEN-SEC
      *            薬剤の有無
                   IF     (SRY-SRYCD (IDS)(1:1)    =   "6" )
                     OR   (SRY-SRYCD (IDS)(1:3)    =   "059")
                       MOVE    1               TO  FLG-YAKZAI
                   END-IF
      *            保険変更可能かの判定
                   IF      SRY-SRYCD(IDS)(1:3)     =   "101"
                       MOVE    1               TO  FLG-CHK-RSI
                   END-IF
               END-PERFORM
      *        保険変更可能かの判定
               IF      SRY-SRYKBN          =   "40"
                                           OR  "50"
                                           OR  "60"
                                           OR  "80"
                   MOVE    1               TO  FLG-CHK-RSI
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    薬剤で点数がゼロの時、院外処方分とする
           IF      (IDX2                   >   ZERO  )  AND
                   (SPA-COM-SRYKBN(IDX2)   =   "21"  OR  "22"
                                           OR  "23") AND
                   (FLG-YAKZAI             =   1     )  AND
                   (SPA-ZAITEN             =   ZERO  )
               MOVE    1                   TO  SPA-NAI-INGAIKBN
           END-IF
      *
      *    剤の最後に回数入力をセット
      *D   IF      IDX2                >   ZERO
      *D       MOVE    "1"                 TO  SPA-COM-KAIFLG(IDX2)
      *D   END-IF
      *
           MOVE    IDX2                TO  SPA-GMN-MAX
      *
           INITIALIZE                  SPA-J04-TEISEI-AREA
      *
      *    残量廃棄の判定
           PERFORM VARYING     IDX-1 FROM    1   BY  1
                   UNTIL      (IDX-1     >   SPA-MAX-LINE)  OR
                              (IDX-1     >   SPA-GMN-MAX )
      *        残量廃棄の判定
               IF     (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "3" )  AND
                      (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" )
                AND   (SPA-ZAIKBN               NOT =   1   )
                   PERFORM 31034-HAIKI-CHKI-SEC
               END-IF
      *        残量廃棄（注射以下）
               IF     (SPA-HAIKIKBN-FLG    =   "2"   )   AND
                      (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" ) AND
                      (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "4"
                                                    OR  "5"
                                                    OR  "6"
                                                    OR  "7"
                                                    OR  "8" )
                AND   (SPA-ZAIKBN               NOT =   1   )
                   PERFORM 31034-HAIKI-CHKI-SEC
               END-IF
      *        展開時の内容編集（内分泌負荷試験用）
               IF     (SPA-COM-INPUTCD(IDX-1)(1:1) =   "1" ) AND
                      (SPA-COM-SANTEIRRKKBN (IDX-1)   =   1   )
                   ADD     1                   TO  IDX-S
                   MOVE    SPA-COM-INPUTCD(IDX-1)  TO  SPA-J04-TEI-SRYCD
                                                       (IDX-S)
                   MOVE    1                   TO  SPA-J04-TEI-KAISU
                                                       (IDX-S)
               END-IF
           END-PERFORM
      *    自費の時展開時なし
           IF      SPA-COM-SRYKBN(1)   =   "96"  OR  "95"
               INITIALIZE                  SPA-J04-TEISEI-AREA
           END-IF
      *    画像診断のフィルム判定
           PERFORM VARYING     IDX-1   FROM    1   BY 1
                   UNTIL       IDX-1   >   IDX2
               IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )  AND
                      (SPA-COM-DATAKBN (IDX-1)       =   "1" )  AND
                      (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)  AND
                      (SPA-COM-SURYO (IDX-1)         >   1   )
                   IF      FLG-FIRUMU                =   ZERO
                       MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                   ELSE
                       IF      SPA-COM-SURFLG (IDX-1)
                                                   NOT =   "1"
                           MOVE    1           TO  SPA-COM-SURYO
                                                               (IDX-1)
      *                    入力コードの画面用再編集
                           MOVE    IDX-1               TO  IDX
                           PERFORM 3101-INPUTCD-HEN-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           .
       3103-SINKOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄の判定処理
      *****************************************************************
       31034-HAIKI-CHKI-SEC              SECTION.
      *
           IF      (SPA-COM-YKZKBN (IDX-1)  =   "4"  )  AND
                   (SPA-COM-TANICD (IDX-1)  =   "014"
                                            OR  "022"
                                            OR  "046" )  AND
                   (SPA-COM-SURYO(IDX-1)(6:3)  >   ZERO)
               IF     (IDX-1               <   SPA-MAX-LINE  )  AND
                      (SPA-COM-INPUTCD(IDX-1 + 1)  =   "099309901")
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  SPA-COM-AUTOFLG2(IDX-1)
               END-IF
           END-IF
      *
           .
       31034-HAIKI-CHKI-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       31011-SPA-HEN-SEC              SECTION.
      *
      *    診療種別セット
           IF     (IDS                 =   1   )  AND
                  (SRY-RENNUM          =   1   )
               IF      SRY-SRYSYUKBN   NOT =   SPACE
                   ADD     1                   TO  IDX2
                   MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
                   MOVE    SRY-SRYSYUKBN       TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
                   MOVE    SRY-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN
                                                            (IDX2)
                   MOVE    SRY-SRYKBN          TO  SPA-COM-SRYKBN(IDX2)
               END-IF
           END-IF
      *!!
      *    前の行が器材商品の自動発生の時
           IF     (SPA-COM-INPUTCD  (IDX2)(1:3)    =   "058" )  AND
                  (SPA-COM-AUTOKBN2 (IDX2 + 1 )    =   "1" )
               IF      SRY-SRYCD   (IDS)   =   SPA-COM-INPUTCD
                                                        (IDX2 + 1)
                   CONTINUE
               ELSE
      *            登録時と器材コードが違う時、編集なし
                   IF      SRY-MEISKYFLG (IDS) =   "1"
                       MOVE    1               TO  SPA-TENKAI-ERR
                       ADD     1               TO  IDX2
                       MOVE    SRY-SRYSURYO(IDS)
                                               TO  SPA-COM-SURYO (IDX2)
                       MOVE    SRY-SRYSURYO(IDS)
                                               TO  SPA-COM-SURYO2(IDX2)
                       MOVE    "1"             TO  SPA-COM-CUR   (IDX2)
                       GO      TO      31011-SPA-HEN-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           MOVE    SRY-SRYKBN          TO  SPA-COM-SRYKBN    (IDX2)
           MOVE    SRY-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX2)
           MOVE    SRY-SRYCD   (IDS)   TO  SPA-COM-INPUTCD   (IDX2)
           MOVE    SRY-SRYSURYO(IDS)   TO  SPA-COM-SURYO     (IDX2)
           MOVE    SRY-SRYSURYO(IDS)   TO  SPA-COM-SURYO2    (IDX2)
           MOVE    SRY-AUTOKBN (IDS)   TO  SPA-COM-AUTOKBN   (IDX2)
      *    内服逓減の判定
           IF     (SRY-SRYKBN          =   "21" )  AND
                  (SRY-SRYCD (IDS)     =   "820000047" ) AND
                  (SRY-AUTOKBN(IDS)    =   "3"         )
               MOVE    1               TO  SPA-NEIGEN-KBN
           END-IF
      *
      *    約束セットの時、表示なしとする（但し、減は表示する）
           IF     (FLG-YAKUSOKU        =   1   )  AND
                  (SRY-SRYCD (IDS) NOT =   "820000047" )
               MOVE    "1"                 TO  SPA-COM-SET (IDX2)
           END-IF
      *!!
      *    器材商品コード
           IF      SRY-MEISKYFLG (IDS) =   "1"
               MOVE    "1"                 TO  SPA-COM-AUTOKBN2 (IDX2)
           END-IF
      *!!
      *ver.4.5
      *    換算値
           MOVE    SRY-KANSURYO (IDS)  TO  SPA-COM-KANSURYO (IDX2)
      *    関連コメント指示
           IF       SRY-MEISKYFLG (IDS) =   "2"
               MOVE    "1"             TO  SPA-COM-INPUTCONT (IDX2)
           END-IF
      *    内服種類数指示
           IF       SRY-MEISKYFLG (IDS) =   "3"
               MOVE    "1"                TO  SPA-COM-INPUTNAIF (IDX2)
           END-IF
      *
      *    回数を編集
           MOVE    IDX2                TO  IDXS
      *
           IF      SRY-SRYKBN          =   "70"
      *        分画数を編集
               MOVE    SRY-SRYKAISU (IDS)  TO  SPA-COM-BUNGSU (IDX2)
           END-IF
      *    分画数を編集
           IF      SRY-SRYKBN          =   "23"
               MOVE    SRY-SRYKAISU (IDS)  TO  SPA-CHOKIKAISU
               IF      SRY-SRYKAISU (IDS)  >=  28
                   MOVE    1               TO  SPA-CHOKITOFLG
               END-IF
           END-IF
      *
           MOVE    SPA-COM-INPUTCD (IDX2)  TO  SPA-GMN-INPUTCD  (IDX2)
           IF      SRY-AUTOKBN (IDS)   =   "A" OR "B" OR "C"  OR  "D"
                                       OR  "E" OR "F" OR "G"  OR  "H"
               PERFORM 31011-JIKANKBN-SET-SEC
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      SRY-AUTOKBN (IDS)   =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *
      *    約束セットの時、以下処理なし
           IF      SRY-SRYCD(IDS)(1:1) =   "S"
               MOVE    1               TO  FLG-YAKUSOKU
               GO      TO      31011-SPA-HEN-EXT
           END-IF
      *
      *    コメントの時、コメント内容編集
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8" )  AND
                  (SRY-INPUTNUM    (IDS)   NOT =   ZERO         )
               INITIALIZE                  PTCOM-REC
               MOVE    SPA-HOSPNUM          TO  PTCOM-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
               MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
               MOVE    SRY-SRYCD    (IDS)  TO  PTCOM-SRYCD
               MOVE    SRY-INPUTNUM (IDS)  TO  PTCOM-RENNUM
               PERFORM 960-PTCOM-READ-SEC
               IF      FLG-PTCOM           =   ZERO
                   IF      PTCOM-INPUTCOMENT   NOT =   SPACE
                       IF      SRY-SRYCD (IDS)         =   "810000001"
                                                       OR  "008500000"
                                                       OR  "008600000"
                           MOVE    PTCOM-INPUTCOMENT
                                               TO  SPA-GMN-MEISYO-G
                                                                 (IDX2)
                           MOVE    "1"             TO  SPA-COM-MEIFLG
                                                                 (IDX2)
                       ELSE
                           MOVE    PTCOM-INPUTCOMENT
                                               TO  SPA-GMN-MEISYO
                                                                 (IDX2)
                       END-IF
                   END-IF
                   MOVE    PTCOM-INPUTCHI-AREA  TO  SPA-COM-INPUTCHI-G
                                                                (IDX2)
                END-IF
           END-IF
      *
           INITIALIZE                  ORCSC92AREA
           MOVE    IDX2                TO  LNK-SC92-GMN-IDX
           MOVE    "J04"               TO  LNK-SC92-PG
           MOVE    SPA-COM-INPUTCD(IDX2)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NAI-NENREI      TO  LNK-SC92-NENREI
           MOVE    SPA-ROUJIN          TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
      *    フィルムの判定
           IF      SRY-SRYKBN          =   "70"
               IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "7" )  AND
                      (SPA-COM-DATAKBN (IDX2)      =   "3" )
                   MOVE    1               TO  FLG-FIRUMU
               END-IF
           END-IF
      *
      *    コメントの時、コメント内容編集
      *    IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8" )  AND
      *           (SRY-INPUTNUM    (IDS)   NOT =   ZERO         )
      *        INITIALIZE                  PTCOM-REC
      *        MOVE    SPA-HOSPNUM          TO  PTCOM-HOSPNUM
      *        MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
      *        MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
      *        MOVE    SRY-SRYCD    (IDS)  TO  PTCOM-SRYCD
      *        MOVE    SRY-INPUTNUM (IDS)  TO  PTCOM-RENNUM
      *        PERFORM 960-PTCOM-READ-SEC
      *        IF      FLG-PTCOM           =   ZERO
      *            IF      PTCOM-INPUTCOMENT   NOT =   SPACE
      *                IF      SRY-SRYCD (IDS)         =   "810000001"
      *                                                OR  "008500000"
      *                                                OR  "008600000"
      *                    MOVE    PTCOM-INPUTCOMENT
      *                                        TO  SPA-GMN-MEISYO-G
      *                                                          (IDX2)
      *                    MOVE    "1"             TO  SPA-COM-MEIFLG
      *                                                          (IDX2)
      *                ELSE
      *                    MOVE    PTCOM-INPUTCOMENT
      *                                        TO  SPA-GMN-MEISYO
      *                                                          (IDX2)
      *                END-IF
      *            END-IF
      *            MOVE    PTCOM-INPUTCHI-AREA  TO  SPA-COM-INPUTCHI-G
      *                                                         (IDX2)
      *         END-IF
      ***  END-IF
      *
      *    金額入力時、金額へ
      *
      *    自賠責の時、対象外
           IF     (SPA-HKNKBN          =   1    )  AND
      **********  (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (SPA-COM-SRYKBN (IDX2)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(IDX2)(1:5)
                                       =   "09591"  OR
                                           "09592"  OR
                                           "09593"  OR
                                           "09594"  )
               MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
      *        金額入力
               IF     (SPA-COM-SRYSYUKBN (IDX2)    =   "809")
               AND    (SRY-JIHIMONEY  (IDS)        >   ZERO )
                   MOVE    SRY-JIHIMONEY(IDS)  TO  SPA-COM-JIHIMONEY
                                                                  (IDX2)
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
               END-IF
               GO      TO      31011-SPA-HEN-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
               IF      SPA-COM-KISOTEN (IDX2)      =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-JIHIMONEY
                                                                  (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-GMN-KEI   (IDX2)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-GMN-KEI   (IDX2)
               END-IF
      *
      *        自費の時、金額編集
           ELSE
               IF      SPA-COM-SRYKBN (IDX2)   =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-GMN-KEI   (IDX2)
               ELSE
                   MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
               END-IF
           END-IF
      *
           .
       31011-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   時間外区分の画面用再編集処理
      *****************************************************************
       31011-JIKANKBN-SET-SEC          SECTION.
      *
           EVALUATE    SRY-AUTOKBN (IDS)
               WHEN    "A"
                   MOVE    "1"             TO  WRK-TIMEFLG
               WHEN    "B"
                   MOVE    "2"             TO  WRK-TIMEFLG
               WHEN    "C"
                   MOVE    "3"             TO  WRK-TIMEFLG
               WHEN    "D"
                   MOVE    "4"             TO  WRK-TIMEFLG
               WHEN    "E"
                   MOVE    "5"             TO  WRK-TIMEFLG
               WHEN    "F"
                   MOVE    "6"             TO  WRK-TIMEFLG
               WHEN    "G"
                   MOVE    "7"             TO  WRK-TIMEFLG
               WHEN    "H"
                   MOVE    "8"             TO  WRK-TIMEFLG
           END-EVALUATE
           MOVE    SPACE               TO  SPA-GMN-INPUTCD (IDX2)
           MOVE    WRK-TIMEFLG         TO  SPA-GMN-INPUTCD (IDX2)(1:1)
           MOVE    SPA-COM-INPUTCD (IDX2)
                                       TO  SPA-GMN-INPUTCD (IDX2)(3:)
           MOVE    WRK-TIMEFLG         TO  SPA-COM-TIMEFLG (IDX2)
      *
           .
       31011-JIKANKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "J04"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       31011-SURYO-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z19
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
           .
       31011-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       320-SPA-CHKSET-SEC              SECTION.
      *
      *    禁忌薬剤チェック用編集
      *****PERFORM 3202-KNKYAKSET-SEC
      *
           .
       320-SPA-CHKSET-EXT.
           EXIT.
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3202-KNKYAKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX
      *    対象年月−１年を求める
           MOVE    SPA-J02-SRYYMD(1:6) TO  WRK-SRYYM
           COMPUTE WRK-SRYYY           =   WRK-SRYYY     -   1
      *
           PERFORM
                   UNTIL   SPA-J02-SRYYMD(1:6) <   WRK-SRYYM
              PERFORM 3203-KNKYAKSET-HEN-SEC
      *
              ADD     1                    TO  WRK-SRYMM
              IF      WRK-SRYMM            >   12
                   ADD     1                   TO  WRK-SRYYY
                   MOVE    1                   TO  WRK-SRYMM
               END-IF
           END-PERFORM
      *
           .
       3202-KNKYAKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3203-KNKYAKSET-HEN-SEC              SECTION.
      *
      *    診療行為マスターを検索する
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    WRK-SRYYM           TO  SRY-SRYYM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT      =   1          )
      *        投薬・注射を対象とする
               IF       SRY-SRYKBN         =   "21"  OR  "22"  OR
                                               "23"  OR  "31"  OR
                                               "32"  OR  "33"
                   PERFORM 32021-KNK-KOUI-SEC
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3203-KNKYAKSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       32021-KNK-KOUI-SEC              SECTION.
      *
      *    回数を診療会計マスタよりチェックする
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-SRYACCT          =   ZERO  )  AND
                  (ACCT-ZAIKAISU        >   ZERO  )
               CONTINUE
           ELSE
               GO      TO      32021-KNK-KOUI-EXT
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   5   )  OR
                              (SRY-SRYCD(IDX2)     =   SPACE )
               IF      SRY-SRYCD (IDX2)(1:1)   =   "6"
                   PERFORM VARYING     IDX3    FROM    1   BY  1
                           UNTIL       IDX3        >   100
                       IF      SPA-KNK-SRYCD (IDX3)  =   SPACE
                           MOVE    SRY-SRYCD (IDX2)  TO  SPA-KNK-SRYCD
                                                             (IDX3)
                           MOVE    SRY-SRYCD (IDX2)  TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      FLG-TENSU         =   ZERO
                               MOVE    TNS-NAME      TO  SPA-KNK-SRYMEI
                                                               (IDX3)
                           END-IF
                       END-IF
                       IF      SPA-KNK-SRYCD (IDX3)
                                                   =  SRY-SRYCD (IDX2)
                           PERFORM VARYING     IDX4   FROM    1   BY  1
                                   UNTIL       IDX4        >   13
                               IF      SPA-KNK-SRYYM (IDX3 IDX4)
                                                           =   SPACE
                                   MOVE    ACCT-SRYYM     TO
                                                   SPA-KNK-SRYYM
                                                            (IDX3 IDX4)
                                   IF      ACCT-SRYYM
                                                 =  SPA-J02-SRYYMD(1:6)
                                       MOVE    "1"   TO  SPA-KNK-TOGETU
                                                                (IDX3)
                                   END-IF
                                   MOVE    13              TO  IDX4
                               ELSE
                                   IF      SPA-KNK-SRYYM (IDX3 IDX4)
                                                       =   ACCT-SRYYM
                                       MOVE    13              TO  IDX4
                                   END-IF
                               END-IF
                           END-PERFORM
                           MOVE    100             TO  IDX3
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       32021-KNK-KOUI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *    行為セット
      *D       WHEN    "CLICKED"       ALSO    "B03"
      *D           PERFORM 420-KOUISET-SEC
      *    番号検索
      *D       WHEN    "CLICKED"       ALSO    "B04"
      *D           PERFORM 430-BANKENSAKU-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      **    登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 4100-TOROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号入力処理
      *****************************************************************
       4100-HKNCOMBI-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                             (IDY)
                   MOVE    SPA-GMN-THKNCOMBINUM-G (IDY)
                                               TO  SPA-GMN-HKNCOMBI-G
                   MOVE    IDY                 TO  IDX
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "1033"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           ELSE
      *        アフターケアはエラー
               IF     (SPA-NAI-THKNNUM (IDX)       =   "971" )  AND
                      (SPA-NAI-TRSI-HKNKBN (IDX)   =   "3" )
                   MOVE    "9029"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 41001-HKNCOMBI-HEN-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-GMN-HKNCOMBI    TO  SPA-HKNCOMBINUM
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4100-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ変更編集処理
      *****************************************************************
       41001-HKNCOMBI-HEN-SEC               SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-J02-SRYYMD(1:6) TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-GMN-HKNCOMBI    TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    負担率
           MOVE    AGECHK-FTNRATEMEI   TO  SPA-J02-FTNRATEMEI
      *    老人区分
           MOVE    AGECHK-ROUJIN       TO  SPA-ROUJIN
      *    労災
           MOVE    SPA-NAI-TRSI-SISIKBN (IDX)
                                       TO  SPA-RSI-SISIKBN
           MOVE    SPA-NAI-TRSI-HKNKBN (IDX)
                                       TO  SPA-RSI-HKNKBN
      *    自賠責区分
           MOVE    SPACE               TO  SPA-RSI-JUNKYO
           EVALUATE    AGECHK-HKNKBN
               WHEN    4
      *            自賠責健保準拠
                   MOVE    "1"             TO  SPA-RSI-JUNKYO
               WHEN    5
      *            労務災害健保準拠
                   MOVE    "1"             TO  SPA-RSI-JUNKYO
               WHEN    7
      *            第三者行為（自賠責健保準拠）
                   MOVE    "1"             TO  SPA-RSI-JUNKYO
           END-EVALUATE
      *    適用期間判定
           MOVE    ZERO                TO  WRK-YMDS1
           MOVE    ZERO                TO  WRK-YMDS2
           MOVE    SPA-J02-SRYYMD(1:6) TO  WRK-YMDS1(1:6)
           MOVE    SPA-J02-SRYYMD(1:6) TO  WRK-YMDS2(1:6)
           PERFORM   VARYING   IDX-D   FROM    1   BY  1
                     UNTIL     IDX-D       >   31
               IF      SPA-SEL-DAY (IDX-D)     >   ZERO
                   MOVE    IDX-D               TO  WRK-DDS2
                   IF      WRK-DDS1            =   ZERO
                       MOVE    IDX-D               TO  WRK-DDS1
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-J04-SYORI       =   1
               CONTINUE
           ELSE
               IF      (WRK-YMDS1      <   SPA-NAI-TTEKSTYMD (IDX)  OR
                                       >   SPA-NAI-TTEKEDYMD (IDX))
                   OR  (WRK-YMDS2      <   SPA-NAI-TTEKSTYMD (IDX)  OR
                                       >   SPA-NAI-TTEKEDYMD (IDX))
                   MOVE    "1036"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    労災判定
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-THKNKBN  (IDX)  NOT =   SPA-HKNKBN
                   MOVE    1                   TO  FLG-HKNHEN
               END-IF
               MOVE    SPA-NAI-THKNKBN  (IDX)  TO  SPA-HKNKBN
           END-IF
           .
       41001-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC
      *
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF     (MCP-WIDGET(1:7)     =   "INPUTCD" )  OR
                  (MCP-WIDGET(1:6)     =   "MEISYO"  )
               PERFORM 20011-INPUT-IDXSET-SEC
           ELSE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
           END-IF
      *
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       20011-INPUT-IDXSET-SEC               SECTION.
      *
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF      MCP-WIDGET(1:07)    =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:7)     TO  WRK-WIDGET
           END-IF
           IF      MCP-WIDGET(1:6)     =   "MEISYO"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:6)     TO  WRK-WIDGET
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
           END-IF
           .
       20011-INPUT-IDXSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
           MOVE    J04-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
           MOVE    J04-HKTKBN          TO  SPA-GMN-HKTKBN-G
      *
           PERFORM 20021-INPUT-GMNSET-SEC
           .
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードのＳＰＡ編集処理
      *****************************************************************
       20021-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-STR
           MOVE    ZERO                TO  FLG-KUHAKU
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   MAX-GMN
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (J04-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (J04-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
               END-IF
      *
               MOVE    J04-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    J04-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                          (SPA-GMN-IDX)
                   IF     (J04-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      J04-INPUTCD (WRK-IDX2)  NOT =   SPACE
                   IF      WRK-STR             =   ZERO
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20021-INPUT-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC               SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
           END-IF
      *
      *
           .
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC           SECTION.
      *
           IF      SPA-NYUGAIKBN       =   1
               PERFORM 4100-HKNCOMBI-CHK-SEC
           END-IF
      *    包括診療チェック
           PERFORM 4200-HKTKBN-CHK-SEC
      *
      *    画面ＳＰＡチェック処理
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE           TO  FLG-TOROKU
               PERFORM 410112-KOUI-SPACHK-SEC
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       4200-HKTKBN-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-HKTKBN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      SPA-GMN-HKTKBN      =   SPA-GMN-HKTKBN-LST
                                                         (IDY)(1:1)
                   MOVE    SPA-GMN-HKTKBN-LST(IDY)
                                               TO  SPA-GMN-HKTKBN-G
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "1036"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
           .
       4200-HKTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC               SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-INS
           PERFORM VARYING SPA-GMN-IDX     FROM    WRK-STR  BY  1
                   UNTIL   SPA-GMN-IDX     >   SPA-MAX-LINE
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HENDEL-SEC
               END-IF
           END-PERFORM
           IF      FLG-INS             =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
               IF     (FLG-HKNHEN          =   1 )
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD
                                                       (SPA-GMN-IDX)
               END-IF
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
               IF     (SPA-ERRCD       =   SPACE )  AND
                      (FLG-END         =   ZERO  )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               IF      FLG-END         =   1
      *            ＳＰＡの内容を画面へ編集する。カーソル設定なし
                   PERFORM 500-GMNHEN-SEC
               END-IF
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *    行挿入、警告あり
           IF      FLG-INS             =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    点数計算・剤別チェック処理
      **** MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           IF     (SPA-ERRCD       =   SPACE
                                   OR  "0021" )  AND
                  (FLG-END         =   ZERO   )
               CONTINUE
           ELSE
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    診療コード入力
           PERFORM 410119-SAIINPUT-SEC
           .
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-INCNT  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-GMNFLG (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
               IF      WRK-ERRCD           =   SPACE
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    削除時
           IF      LNK-SC97-INPUTCD(1:1)   =   SPACE
      *
      *        セットが削除の時セット内容を削除する
               IF      SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF
      *
      *    剤削除時
      *    '-'入力がある時、剤削除ができなので、エラーとする
           IF      LNK-SC97-INPUTCD(1:1)   =   "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    "1030"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
           END-IF
      *
           .
       41011-KOUI-HENDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-HEN2-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
           MOVE    LNK-SC97-NYURYOKU   TO  FLG-NYURYOKU
           MOVE    LNK-SC97-INCD       TO  WRK-CD
           MOVE    LNK-SC97-INCDCNT    TO  WRK-CD-MCNT
      *    '_'を削除した時、クリアする
           IF      LNK-SC97-JODOU      >   ZERO
               MOVE    "3"             TO  SPA-COM-IN  (SPA-GMN-IDX)
           END-IF
      *
      *    行挿入
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "+"
               MOVE    SPACE           TO  WRK-HEN-INPUTCD
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)
                                       TO  WRK-HEN-INPUTCD
               MOVE    WRK-HEN-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41014-GYOINSN-SEC
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               ADD     1               TO  SPA-GMN-IDX
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    一覧へ
           IF      LNK-SC97-INPUTCD(1:2)
                                       =  "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-J98-AREA
               MOVE    LNK-SC97-INPUTCD    TO  SPA-J98-INPUTCD
               PERFORM 410-J98-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    Ｐ入力へ
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-J99-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
           IF     LNK-SC97-NYURYOKU    =   1
      *///     セットコードに変更がない時
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1) =   "S" ) AND
                      (SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                           =   SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX))
                   MOVE    1                   TO  FLG-SET-HEN
               ELSE
                   MOVE    ZERO                TO  FLG-SET-HEN
      *            コードが同一の時
                   IF    (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:6)
                                               =   SPA-GMN-INPUTCD
                                                   (SPA-GMN-IDX)(1:6))
                       MOVE    2               TO  FLG-SET-HEN
                   END-IF
               END-IF
      *        セットが変更削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S" ) OR
                      ((SPA-GMN-IDX            <   SPA-MAX-LINE )  AND
                       (SPA-COM-SET  (SPA-GMN-IDX + 1)     =   "1"))
                   PERFORM 41016-SETDEL-SEC
               END-IF
               IF      FLG-SET-HEN         =   ZERO
                                           OR  2
      *        セットコードが同じ時、禁忌チェック済みを保存する
               IF      FLG-SET-HEN         =   2
                   MOVE    SPA-COM-KINKIFLG (SPA-GMN-IDX)
                                               TO  WRK-KINKIFLG
                   MOVE    SPA-COM-PTKINKI-ARI (SPA-GMN-IDX)
                                               TO  WRK-PTKINKI-ARI
               ELSE
                   MOVE    SPACE               TO  WRK-KINKIFLG
                   MOVE    ZERO                TO  WRK-PTKINKI-ARI
               END-IF
      *
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)    TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)    TO  WRK-FLG-IN
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
      *
               MOVE    WRK-KINKIFLG    TO  SPA-COM-KINKIFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-PTKINKI-ARI TO  SPA-COM-PTKINKI-ARI
                                                        (SPA-GMN-IDX)
      *
               END-IF
           ELSE
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
           END-IF
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
      *    診療行為一覧へ
           IF      LNK-SC97-KENSAKU    =   "1"
               INITIALIZE                      SPA-J98-AREA
               MOVE    LNK-SC97-INCD       TO  SPA-J98-INPUTCD
               MOVE    "1"                 TO  SPA-J98-TYPE
               MOVE    LNK-SC97-CDSYU      TO  SPA-J98-CDSYUBETU
               MOVE    "1"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-J98-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    LNK-SC97-INPUTCD(1:4)   TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
           ELSE
      *        文字変換・編集処理
               PERFORM 410112-INPUTCD-HENKAN-SEC
      *
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX
           MOVE    ZERO                TO  WRK-IN-IDX2
           MOVE    ZERO                TO  WRK-MAX-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
               IF     (SPA-COM-SET (IDX)   =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)    NOT =   SPACE )
                   MOVE    IDX             TO  WRK-MAX-IDX
               END-IF
      *        薬評・器評の数量なしとする
               IF     (SPA-COM-IN2     (IDX)   =   "2" )
                  AND (SPA-COM-YAKUHYO (IDX)   =   "1" )
                   IF    (SPA-COM-INPUTCD (IDX)(1:1)  =   "6"
                                                      OR  "7" )
                     OR  (SPA-COM-INPUTCD (IDX)(1:3)  =   "059" )
                       MOVE    SPACE           TO  SPA-COM-IN2 (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-IN-IDX          =   ZERO
      ******** MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
           IF      SPA-GMN-IDX         =   ZERO
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *
      *    数量・回数入力なし
           IF     (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "1"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001" )
                  AND (SPA-COM-YCOMKBN (SPA-GMN-IDX)       =   ZERO  )
                   MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               ELSE
                   STRING  WRK-INPUTCD          DELIMITED  BY  SPACE
                           "_"                  DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   END-STRING
               END-IF
               MOVE    SPACE                TO  SPA-ERRCD
           END-IF
      *    名称入力へのカーソル移動
           IF      WRK-IN-IDX          >   ZERO
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "81"  OR  "83") OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083"  )
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0085"  )
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0086"  )
      *R02.4
                   IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                           =   "831"   )
                       CONTINUE
                   ELSE
                       MOVE    "3"         TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF      FLG-KUHAKU          =   1
      *        空白行入力時、最終行へ
               MOVE    "1"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
           END-IF
      *
           .
       410119-SAIINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列変換・編集処理
      *****************************************************************
       410112-INPUTCD-HENKAN-SEC      SECTION.
      *
      *    変換文字列編集
           PERFORM 4101121-MOJI-SET-SEC
      *
      *    セット入力処理へ
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "S"  OR  "P"
               PERFORM 410119-INPSET-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *
      *    入力内容チェック、編集
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    LNK-SC97-INCD   TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           END-IF
           PERFORM 4101113-INPUTCD-CHK-SEC
      *    名称チェック
           PERFORM 40113-MEISYOU-CHK-SEC
      *
           .
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC       SECTION.
      *
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               CONTINUE
           ELSE
      *        名称を戻す
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "1"
                   MOVE    SPA-COM-NAME(SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                        (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX)
      *    入力値の入力判定用
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "5"                 TO  LNK-SC92-KBN
           END-IF
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    ZERO                TO  LNK-SC92-TIMEFLG
      *
           MOVE    "J04"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NAI-NENREI      TO  LNK-SC92-NENREI
           MOVE    SPA-ROUJIN          TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *****MOVE    LNK-SC92-TIMEFLG    TO  SPA-NAI-TIMEFLG
      *    カーソル位置設定
           PERFORM 41011139-INFLG-HEN-SEC
      *
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
      *            算定回数、併用算定チェックはエラーとしない
                   IF     IDX-ERR              =   3  OR  6
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  AND
                  (LNK-SC92-ERRMSG NOT =   SPACE )
               MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
           END-IF
      *    警告判定
           IF      SPA-ERRCD(1:1)      =   "K"
             AND  (SPA-ERRCD       NOT =   "K033" )
               IF      SPA-COM-KZMFLG (SPA-GMN-IDX)  =   SPACE
                   MOVE    "1"             TO  SPA-COM-KZMFLG
                                                          (SPA-GMN-IDX)
                   MOVE    "2"             TO  SPA-COM-CUR(SPA-GMN-IDX)
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-COM-CUR(SPA-GMN-IDX)
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI4 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF
      *!!  保険適用区分が保険適用外のもの（ユーザー登録の自費分）
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
      *        数量を金額とする
      *        IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
               IF      SPA-COM-TEN     (SPA-GMN-IDX)  =   ZERO
                   MOVE    1               TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-KEI(SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-JIHIMONEY
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-GMN-KEI(SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
           END-IF
      *
           .
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置設定処理
      *****************************************************************
       41011139-INFLG-HEN-SEC       SECTION.
      *
      *    数量以降入力なし（’＿’編集用）
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )
               MOVE    ZERO                TO  FLG-IN1
      *        薬剤と数量入力必須の診療行為、薬剤コメント
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
                   MOVE    "7"                     TO  WRK-MOJI
               ELSE
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                                   TO  WRK-MOJI
               END-IF
      *        数量の入力の有無
               IF     (WRK-MOJI                    =   "6" OR
                                                       "7" )  OR
                     ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001" )
      *                  用量コメント
                 OR  ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992000" )
      *            薬剤・器材・用法
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF      WRK-MOJI                =   "1"
                   IF  (SPA-COM-SRYSYUKBN(SPA-GMN-IDX)(1:1)
                                                    NOT =   "7" ) AND
                       (SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                                    NOT =   0   )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" )  OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084")
      *            コメント
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                                   =   "09500" OR
                                                       "09600" )
      *            自費
                   IF     (SPA-COM-TEN (SPA-GMN-IDX)   =   ZERO )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0959"  )
      *            自費
                   IF     (SPA-COM-TEN (SPA-GMN-IDX)   =   ZERO )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
      *        用法コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "001" )
                 AND  (SPA-COM-YCOMKBN(SPA-GMN-IDX) =   1 )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *H30.4   分割調剤など
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                               =   "0992081" )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *R02.4
      *        コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "85" ) 
                 OR   (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "831" ) 
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *
               IF      FLG-IN1             =   1
                   MOVE    "2"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN3 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       41011139-INFLG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81" )
               OR (SPA-COM-INPUTCD (SPA-GMN-IDX)       =  "008500000" )
               OR (SPA-COM-INPUTCD (SPA-GMN-IDX)       =  "008600000" )
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "83" OR
                                                         "84")  OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0083" OR
                                                         "0084")
             OR   (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0085")
             OR   (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0086")
      *R02.4
             OR   (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "85"  )
               IF     (LNK-SC97-NYURYOKU   =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )  AND
                     ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83"   )  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083" )
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0085" )
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0086" ))
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
                                                       OR "83"
                                                       OR "84") OR
      *R02.4
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  =  "85"  ) OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  =  "0083"
                                                       OR "0084"
                                                       OR "0085"
                                                       OR "0086")) AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX) NOT =   SPACE)
      *        半角の空白を全角に置きかえる
               PERFORM 401131-MEISYO-HENKAN-SEC
           END-IF
      *
           .
       40113-MEISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称チェック・全角置換処理
      *****************************************************************
       401131-MEISYO-HENKAN-SEC        SECTION.
      *
      *    半角の空白を全角に置きかえる
      *    フリーコメントは１桁目から
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)   =   "810000001"
                                                  OR  "008500000"
                                                  OR  "008600000"
               MOVE    1                   TO  IDX-STR
      *        MOVE    80                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
           ELSE
               MOVE    3                   TO  IDX-STR
      *!!      MOVE    78                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                           =   "831" 
                   MOVE    138                 TO  WRK-LEN
               END-IF
      *
               IF     (SPA-GMN-MEISYO-G(SPA-GMN-IDX)(1:2)
                                           =   "* "
                                           OR  SPACE    )
                   CONTINUE
               ELSE
      *            開始位置より前に入力があれば元に戻してエラー
                   MOVE    "0100"          TO  SPA-ERRCD
                   MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   MOVE    2               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
                   MOVE    SPA-COM-NAME (SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                           (SPA-GMN-IDX)
               END-IF
           END-IF
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
                                                            (IDX-STR:)
           END-IF
      *
           .
       401131-MEISYO-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-J99-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-J99GMN-AREA
           MOVE    SPACE               TO  J99-PIN
           MOVE    SPACE               TO  J99-PED
      *
      *    画面カーソルセット処理
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  SPA-J99-DATA
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "J99"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-J99-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-J98-SYORI-SEC          SECTION.
      *
      *    内服を編集する
           INITIALIZE                  SPA-J98GMN-AREA
      *!!  IF      SPA-J98-INPUTCD         =   SPACE
           IF      SPA-J98-INPUTCD(1:2)    =   "//"
               MOVE    SPA-J98-INPUTCD(3:)     TO  SPA-J98-GMN-INPUT
               EVALUATE    SPA-J98-INPUTCD(3:1)
      *        コメント一覧
                   WHEN    "C"
                   WHEN    "c"
                       MOVE    "U"             TO  SPA-J98-GMN-SYORI
                       MOVE    "U4"            TO  SPA-J98-GMN-SYORI3
                       MOVE    SPACE               TO  SPA-J98-INPUTCD
      *        用法
                   WHEN    "Y"
                   WHEN    "y"
                       MOVE    "U"             TO  SPA-J98-GMN-SYORI
                       MOVE    "U1"            TO  SPA-J98-GMN-SYORI3
                       MOVE    SPACE               TO  SPA-J98-INPUTCD
      *        診療種別一覧
                   WHEN    "."
                       MOVE    "."             TO  SPA-J98-INPUTCD
      *            区分番号別
                   WHEN    "*"
                       MOVE    SPA-J98-GMN-INPUT   TO  SPA-J98-INPUTCD
                   WHEN    "/"
                       MOVE    SPA-J98-GMN-INPUT   TO  SPA-J98-INPUTCD
      *H25.6       労災一覧
                   WHEN    "R"
                   WHEN    "r"
                       MOVE    SPA-J98-INPUTCD     TO  SPA-J98-GMN-INPUT
      *
      *H30.9
      *        選択式コメント一覧
               WHEN    "S"
               WHEN    "s"
                   IF      SPA-SRYYMD      <   "20180401"
                       MOVE    "1107"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-J98-GMN-INPUT
                   ELSE
                       PERFORM 41019-J98-RECEKISAI-SEC
                   END-IF
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO  410-J98-SYORI-EXT
                   END-IF
      *
                   WHEN    OTHER
                       MOVE    SPACE               TO  SPA-J98-INPUTCD
               END-EVALUATE
      *        画面編集
               PERFORM 4101-J98-SET-SEC
           ELSE
               INITIALIZE                  SPA-J98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-J98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)  =  "S")
      *            内服薬剤
                   MOVE    "3"                 TO  SPA-J98-GMN-SYORI
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(SPA-GMN-IDX - 1)
                       WHEN    SPACE
      *                    診療区分が決定してない時
                           PERFORM  4101-J98-SYRKBN-SEC
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
      *                    注射
                           MOVE    "5"         TO  SPA-J98-GMN-SYORI
                       WHEN    "23"
      *                    外薬
                           MOVE    "4"         TO  SPA-J98-GMN-SYORI
                       WHEN    "21"
                       WHEN    "22"
      *                    外薬
                           MOVE    "3"         TO  SPA-J98-GMN-SYORI
                       WHEN    OTHER
      *                    診療行為
      ******************   MOVE    "3"         TO  SPA-J98-GMN-SYORI
                           MOVE    "3"         TO  SPA-J98-GMN-SYORI
                           MOVE    1           TO  FLG-KOUI
                   END-EVALUATE
               END-IF
      *        画面編集
               PERFORM 4101-J98-SET-SEC
      *
      *
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-J98-GMN-MAX     =   ZERO )  AND
      ************    (SPA-J98-TYPE        =   "2"  )  AND
      ************    (SPA-J98-GMN-SYORI   =   "8"  )
                      (SPA-J98-CDSYUBETU   =   "J"  )  AND
                      (FLG-KOUI            =   1    )
                   INITIALIZE                  SPA-J98GMN-AREA
                   MOVE    "8"                 TO  SPA-J98-GMN-SYORI
      *            画面編集
                   PERFORM 4101-J98-SET-SEC
               END-IF
           END-IF
      *
      *****点数マスタ編集で、対象データなしの時、診療行為で検索する
      *    IF     (SPA-J98-GMN-MAX     =   ZERO )  AND
      *           (SPA-J98-TYPE        =   "2"  )
      *        INITIALIZE                  SPA-J98GMN-AREA
      *        MOVE    "8"                 TO  SPA-J98-GMN-SYORI
      *        画面編集
      *        PERFORM 4101-J98-SET-SEC
      *    END-IF
      *    対象データなし
      *****IF      SPA-J98-GMN-MAX     =   ZERO
      *        MOVE    "0001"              TO  SPA-ERRCD
      *        MOVE    1                   TO  SPA-GMN-CUR
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      410-J98-SYORI-EXT
      *****END-IF
      *
      *    画面カーソルセット処理
           IF      SPA-J98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "J98"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "J04"               TO  SPA-J98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410-J98-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分が決定してない時診療種別処理
      *****************************************************************
       4101-J98-SYRKBN-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IDX
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:3)
                   WHEN    ".21"
                       MOVE    "3"                 TO  SPA-J98-GMN-SYORI
                   WHEN    ".22"
                       MOVE    "3"                 TO  SPA-J98-GMN-SYORI
                   WHEN    ".23"
                       MOVE    "4"                 TO  SPA-J98-GMN-SYORI
                   WHEN    ".31"
                   WHEN    ".32"
                   WHEN    ".33"
                       MOVE    "5"                 TO  SPA-J98-GMN-SYORI
               END-EVALUATE
           END-PERFORM
      *
           .
      *
       4101-J98-SYRKBN-EXT.
           EXIT.
      *
      *H30.9
      *****************************************************************
      *    選択式コメント一覧編集処理
      *****************************************************************
       41019-J98-RECEKISAI-SEC       SECTION.
      *
           IF       SPA-J98-INPUTCD(4:1)   =   "S"
                                           OR  "s"
               MOVE    "R"             TO  SPA-J98-GMN-SYORI
               MOVE    SPACE           TO  SPA-J98-INPUTCD
               MOVE    SPACE           TO  SPA-J98-GMN-SYORI2
               MOVE    "2"             TO  SPA-J98-TYPE
           ELSE
               MOVE    "C"             TO  SPA-J98-GMN-SYORI
               MOVE    SPACE           TO  SPA-J98-INPUTCD
               IF     (SPA-SELCOM-IDX  =   ZERO )
                 AND  (SPA-GMN-IDX     >   1    )
                   PERFORM 510112-SELCOM-CHK-SEC
               END-IF
               IF     (SPA-SELCOM-IDX  =   ZERO  )
                   MOVE    SPACE           TO  SPA-J98-GMN-SYORI
                   MOVE    SPACE           TO  SPA-J98-INPUTCD
                   MOVE    "1106"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-COM-INPUTCD (SPA-SELCOM-IDX)
                                               TO  SPA-J98-SELCOM-SRYCD
               END-IF
           END-IF
           .
       41019-J98-RECEKISAI-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-J98-SET-SEC       SECTION.
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J04             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J04
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
           .
      *
       4101-J98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101121-MOJI-SET-SEC        SECTION.
      *
      *
           MOVE    LNK-SC97-SURYOCNT   TO  WRK-SUR-MCNT
           MOVE    LNK-SC97-KAISUCNT   TO  WRK-ACCT-MCNT
           MOVE    LNK-SC97-SURYO      TO  WRK-SURYO
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
      *****MOVE    LNK-SC97-KAISUFLG   TO  FLG-KAISU
           MOVE    LNK-SC97-BUNGSU     TO  WRK-BUNGSU
           MOVE    LNK-SC97-MCNT1      TO  WRK-MCNT1
           MOVE    LNK-SC97-MCNT2      TO  WRK-MCNT2
           MOVE    LNK-SC97-MCNT3      TO  WRK-MCNT3
           MOVE    LNK-SC97-MCNT4      TO  WRK-MCNT4
           MOVE    LNK-SC97-MOJI1      TO  WRK-MOJI1
           MOVE    LNK-SC97-MOJI2      TO  WRK-MOJI2
           MOVE    LNK-SC97-MOJI3      TO  WRK-MOJI3
           MOVE    LNK-SC97-MOJI4      TO  WRK-MOJI4
      *
      *    早朝加算区分の施設規準チェック
      *    IF      LNK-SC97-JIKAN      =   8
      *        IF     (SPA-YAKANKBN        =   1  )
      *            CONTINUE
      *        ELSE
      *            MOVE    "0210"          TO  SPA-ERRCD
      *        END-IF
      *    END-IF
      *
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力チェック処理
      *****************************************************************
       4101121-INPUTCD-CHECK-SEC    SECTION.
      *
      *    回数入力不可(J04)
           IF      WRK-ACCT-MCNT   NOT =   ZERO
               MOVE    "1021"          TO  SPA-ERRCD
               GO      TO      4101121-INPUTCD-CHECK-EXT
           END-IF
      *
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                           TO  WRK-MOJI
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
               MOVE    "7"                 TO  WRK-MOJI
           END-IF
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "008"
               MOVE    "8"                 TO  WRK-MOJI
           END-IF
      *
           EVALUATE    WRK-MOJI
      *
      *        診療行為
               WHEN    "1"
      *            きざみ値計算識別判定がない時、数量入力エラー(29)
                   IF      TNS-KZMCOMPSIKIBETU     =   0
                       IF      WRK-SUR-MCNT    NOT =   ZERO
                           MOVE    "0002"          TO  SPA-ERRCD
                       END-IF
      ************ ELSE
      *                数量　＞　きざみ値上限の時、エラー
      *                IF      SPA-COM-SURYO(SPA-GMN-IDX)
      *                                            >  TNS-KZMJGN
      *                    MOVE    "0037"          TO  SPA-ERRCD
      **************   END-IF
                   END-IF
      *
      *
      *        医薬品
      *        WHEN    "6"
      *            IF      WRK-SUR-MCNT        =   ZERO
      *                MOVE    "0003"          TO  SPA-ERRCD
      *            END-IF
      *
      *        特定器材
               WHEN    "7"
      *            1行目入力不可
                   IF      SPA-GMN-IDX     =   1
                       MOVE    "0013"          TO  SPA-ERRCD
                   END-IF
      *
      *        コメント
               WHEN    "8"
      *            数量をゼロとする
                   MOVE    ZERO            TO  WRK-SUR-MCNT
      *
               WHEN    OTHER
                   CONTINUE
           END-EVALUATE
      *
      *    時間加算チェック
           IF     (SPA-ERRCD           =   SPACE   )  AND
                  (LNK-SC97-JIKAN  NOT =   ZERO    )
      *        時間加算区分
      *        診療で、手技料の時
               IF     (LNK-SC97-JIKAN      =   1   OR  2  OR  3
                                                   OR  4
                                                   OR  5
                                                   OR  6
                                                   OR  7       )  AND
                      (TNS-DATAKBN         =   "1"             )
                  MOVE    LNK-SC97-JIKAN       TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    LNK-SC97-JIKAN      TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
           END-IF
           .
       4101121-INPUTCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           MOVE    SPACE               TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
           IF      WRK-SUR-MCNT        =   ZERO
               MOVE    1               TO  SPA-COM-SURYO(SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-SURFLG(SPA-GMN-IDX)
           ELSE
               MOVE    WRK-SURYO       TO  SPA-COM-SURYO(SPA-GMN-IDX)
               IF      SPA-COM-SURYO(SPA-GMN-IDX)  =  ZERO
                   MOVE    1           TO  SPA-COM-SURYO(SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-SURFLG(SPA-GMN-IDX)
               IF      LNK-SC97-AUTOFLG2   =   "1"
                   MOVE    "1"         TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-SURYO(SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               IF      SPA-COM-KAISU(SPA-GMN-IDX)  =  ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
               MOVE    SPACE           TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *    分画数（未入力時、１回）
           MOVE    WRK-BUNGSU          TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           IF      WRK-BUNGSU          =   ZERO
               MOVE    1               TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           END-IF
      *
      *    入力値の入力判定用
      *    その他値
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4
      *
           MOVE    LNK-SC97-MOJI1      TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
           MOVE    LNK-SC97-MOJI2      TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
           MOVE    LNK-SC97-MOJI3      TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
           MOVE    LNK-SC97-MOJI4      TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
      *
           .
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *!!  約束セット以外をエラーとする
      *    IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
      *        CONTINUE
      *    ELSE
      *        MOVE    "1022"              TO  SPA-ERRCD
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      410119-INPSET-EXT
      *!!  END-IF
      *
      *    時間外がないこと
      *    IF      FLG-JKNKSN      NOT =   ZERO
      *        MOVE    "0010"              TO  SPA-ERRCD
      *        GO  TO                 410119-INPSET-EXT
      *    END-IF
      *
      *    約束の時、１行前に診療種別があること
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF     (SPA-GMN-IDX         =   1   )   OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)
                                       NOT = "." )
                   MOVE    "0048"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
      *************GO      TO      410119-INPSET-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       410119-INPSET-EXT.
           EXIT.
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       4101191-INPUTSET-SYORI-SEC      SECTION.
      *
      *    約束処方の初期処理
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101191-INPUTSET-SYORI-EXT
           END-IF
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "J04"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
      *    ORCSC92検索用
           MOVE    SPA-NAI-NENREI      TO  ORCSCSETHEN-NENREI
           MOVE    SPA-ROUJIN          TO  ORCSCSETHEN-ROUJIN
           MOVE    SPACE               TO  ORCSCSETHEN-LSTSRYKA
           MOVE    SPACE               TO  ORCSCSETHEN-LSTSRYKAMEI
      *
           CALL    "ORCSCSETHEN"       USING
                                       ORCSCSETHENAREA
                                       SPA-AREA
                                       SPA-SCR-REC
      *    展開後の位置
           MOVE    ORCSCSETHEN-IDX     TO  SPA-GMN-IDX
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    ORCSCSETHEN-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *
           .
       4101191-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力文字を数字へ変換
      *    MOVE    1                   TO  WRK-IDX-FT
      *    PERFORM 4101121-MOJI-SET-SEC
      *
      *    入力位置のセット
           MOVE    ZERO                TO  WRK-IN-SURYO
           MOVE    SPA-GMN-IDX         TO  WRK-IN-IDX
      *    数量以降入力なし
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   MOVE    1                   TO  WRK-IN-SURYO
                   MOVE    "1"                 TO  SPA-COM-IN
                                                      (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC
           MOVE    WRK-CD(1:WRK-CD-MCNT)   TO  WRK-INPUTCD
      *
      *    約束名称編集
           PERFORM 41011921-INPSET-MEI-SEC
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       41011921-INPSET-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM          TO  ICD-HOSPNUM
      *
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    WRK-INPUTCD             TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       41011921-INPSET-MEI-EXT.
           EXIT.
      *
      *H30.9
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       510112-SELCOM-CHK-SEC       SECTION.
      *
           COMPUTE IDX-STR             =   SPA-GMN-IDX  -  1
           PERFORM VARYING    IDX      FROM    IDX-STR  BY  -1
                   UNTIL     (IDX      <       1   )
                        OR   (SPA-SELCOM-IDX   >   ZERO  )
               IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )
                  AND (SPA-COM-RECEKISAI(IDX)      =   "1"
                                                   OR  "2"  )
                   MOVE    IDX                 TO  SPA-SELCOM-IDX
                   MOVE    1                   TO  SPA-SELCOM-FLG
               END-IF
      *        同じ剤内での検索
               IF      (IDX            NOT =   IDX-STR )
                   AND (SPA-COM-ZAIEND (IDX)   =   "1" )
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           .
       510112-SELCOM-CHK-EXT.
           EXIT.
      *
      *未使用（自動表示なし）
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       51011-SELCOM-J98-SEC       SECTION.
      *
           MOVE    SPA-SELCOM-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-J98-AREA
           MOVE    "C"                 TO  SPA-J98-GMN-SYORI
           MOVE    "//S"               TO  SPA-J98-INPUTCD
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-SELCOM-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-J98-SYORI-SEC
           .
       51011-SELCOM-J98-EXT.
           EXIT.
      *TEST
      *TEST
      *!!!!
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
      *    空白行をなくす
           PERFORM 420-SYOKYO-SEC
      *
      *    剤分離処理
           INITIALIZE                  ORCSC93AREA
           MOVE    "J04"               TO  LNK-SC93-PG
      *    小児科外来診療科があり、３歳満の時
           IF      (SPA-SYONIKA        =   1   )  AND
                   (SPA-NAI-NENREI-YY  <   3   )
               MOVE    1                   TO  LNK-SC93-SYONIKA-ARI2
           END-IF
      *    寝たきり在宅科算定有りとする
           MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-SC93-HAIKIKBN-FLG
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF      SPA-NYUGAIKBN       =   1
               MOVE    SPA-COM-SRYKBN(01)  TO  SPA-J04-SRYKBN
           END-IF
           IF      (SPA-J04-SRYKBN NOT =   SPA-COM-SRYKBN(01))
              AND  (SPA-NYUGAIKBN      =   2                 )
               MOVE    "1020"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR  (01)
               MOVE    SPACE           TO  SPA-MAE-INPUTCD (01)
               GO      TO      510-TBLHEN-EXT
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *!!
      *    内分泌負担試験編集
           MOVE    ZERO                TO  FLG-NAIFUTAN
      *H30.4
      *    妊婦判定
           MOVE    ZERO                TO  FLG-NINPUKBN-IN
      *
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE )  AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE ))
      *        内分泌負担試験有無
               IF     (SPA-COM-SRYKBN (IDX)    =   "60"  )  AND
                      (SPA-COM-DATAKBN(IDX)    =   "1"   )  AND
                      (SPA-COM-HOUKNSKBN(IDX)  =   08    )
                   MOVE    1               TO  FLG-NAIFUTAN
               END-IF
      *H30.4
      *        妊婦判定
               IF     (SPA-COM-SRYKBN (IDX)    =   "11"
                                               OR  "12"
                                               OR  "13" )
                  AND (SPA-COM-INPUTCD (IDX)(1:1)  =   "1")
      *            妊婦加算判定
                   SET     TBL-NPIDX18         TO  1
                   SEARCH  TBL-NINPUKSN18-OCC  VARYING   TBL-NPIDX18
                       AT   END
                           CONTINUE
                       WHEN   (TBL-NP18-SRYCD (TBL-NPIDX18)
                                           =   SPA-COM-INPUTCD (IDX))
                       MOVE    1               TO  FLG-NINPUKBN-IN
                   END-SEARCH
      *
               END-IF
      *
           END-PERFORM
           IF      FLG-NAIFUTAN        =   1
               INITIALIZE                  ORCSSANFUKAAREA
               MOVE    2                   TO  LNK-SANFUKA-SYORI
               MOVE    "J04"               TO  LNK-SANFUKA-PG
               MOVE    SPA-J02-SRYYMD      TO  LNK-SANFUKA-SRYYMD
      *        訂正時の診療コード
               MOVE    SPA-J04-TEISEI-AREA TO  LNK-SANFUKA-TEISEI-AREA
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *!!
      *
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
      *    剤ごとの処理
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HZN-SCR-AREA
           INITIALIZE                  WRK-HOZ-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )  OR
                              (SPA-ERRCD           NOT =   SPACE )
      *        剤ごとの算定処理で変更される加算データは対象としない
      *DDDDDDDDIF      SPA-COM-AUTOKBN (IDX)    =   "2"
      *DD          CONTINUE
      *DD      ELSE
               IF     ((SPA-COM-SRYKBN (IDX)   =   "11"  OR  "12") AND
                       (SPA-COM-AUTOKBN(IDX)   =   "2"   )  )  OR
      *            注射の残量廃棄も対象外
                      ((SPA-COM-SRYKBN (IDX)(1:1)  =   "3") AND
                       (SPA-COM-AUTOKBN(IDX)   =   "2"   )  AND
                       (SPA-COM-INPUTCD(IDX)   =   "099309901"))
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-Z
                   MOVE    SPA-COM-TBLREC (IDX)
                                           TO  WRK-COM-TBLREC(IDX-Z)
                   MOVE    SPA-GMN-TBLREC (IDX)
                                           TO  WRK-GMN-TBLREC(IDX-Z)
               END-IF
      *DDDDDDDDEND-IF
      *        回数編集
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
                   IF      SPA-COM-KAIFLG (IDX)    =   "1"
                        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                 外用薬の時、入力回数を
                        IF     (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                               (SPA-COM-KAISU (IDX) =   1    )
                            MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                        END-IF
                   END-IF
               END-IF
      *
               IF     (IDX                 =   SPA-GMN-MAX   )  OR
                      (SPA-COM-ZAIEND (IDX)
                                           =   "1"           )
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
               END-IF
           END-PERFORM
      *@@
      *検査一覧表示へ
           IF      SPA-ERRCD           =   "AK98"
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    1                   TO  SPA-AKUSEI-FLG
           ELSE
               MOVE    ZERO                TO  SPA-AKUSEI-FLG
           END-IF
      *@@
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (WRK-HZN-GMN-INPUTCD (IDX)   =   SPACE)  AND
                      (WRK-HZN-COM-INPUTCD (IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
                   MOVE    WRK-HZN-GMN-TBLREC (IDX)
                                               TO  SPA-GMN-TBLREC
                                                         (SPA-GMN-MAX)
                   MOVE    WRK-HZN-COM-TBLREC (IDX)
                                               TO  SPA-COM-TBLREC
                                                         (SPA-GMN-MAX)
      *@
      *            検査一覧表示へ
                   IF      SPA-AKUSEI-FLG      =   1
                       IF    SPA-COM-CUR (SPA-GMN-MAX)  =  "A"
                           MOVE    SPA-GMN-MAX     TO  SPA-AKUSEI-IDX
                           MOVE    SPACE           TO  SPA-COM-CUR
                                                       (SPA-GMN-MAX)
                       END-IF
                    END-IF
      *@
               END-IF
      *!!
      *        出来高診療
               IF      SPA-COM-INPUTCD (SPA-GMN-MAX)
                                               =   "099999903"
                   MOVE    SPA-GMN-HKTKBN-LST (1)
                                               TO  SPA-GMN-HKTKBN-G
               END-IF
      *!!
      *        包括算定（剤）
               IF      SPA-COM-INPUTCD (SPA-GMN-MAX)
                                               =   "099999908"
                   MOVE    SPA-GMN-HKTKBN-LST (2)
                                               TO  SPA-GMN-HKTKBN-G
               END-IF
      *!!
           END-PERFORM
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   MAX-GMN    +    1
           END-IF
      *
      *    悪性腫瘍検査一覧へ
           IF      SPA-AKUSEI-FLG      =   1
               PERFORM 5101-AKUSEI-J98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *
      *    禁忌チェックを行う
           IF      FLG-TOROKU          =   SPACE
               PERFORM 51010-KNKCHK-SEC
               IF      FLG-KINKI           =   1
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF
      *
      *    登録前の全体のチェックを行う
      *D   IF      FLG-TOROKU      NOT =   "2"
      *D       INITIALIZE                  ORCSC00AREA
      *D       MOVE    "2"                 TO  LNK-ORCSC-KBN
      *D       PERFORM 520-ALL-CHEK-SEC
      *D       IF      SPA-ERRCD   NOT =   SPACE
      *D           DISPLAY "WRK-COM-KEIFLG:" WRK-COM-KEIFLG (3)
      *D       END-IF
      *D   END-IF
           .
        510-TBLHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    剤ごとの処理
      *****************************************************************
       5102-ZAIBETU-SEC             SECTION.
      *
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
           INITIALIZE                  ORCSC00AREA
           MOVE    "1"                 TO  LNK-ORCSC-KBN
           MOVE    "J04"               TO  LNK-ORCSC-PG
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
      **** MOVE    SPA-MAP-IDX         TO  LNK-ORCSC-MAP-IDX
      *****MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-ROUJIN          TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NAI-NENREI      TO  LNK-ORCSC-NENREI 
      *D   MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
      *D   MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
      *D   MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-NAI-INGAIKBN    TO  LNK-ORCSC-INGAIKBN
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *    小児科外来診療科があり、３歳満の時
           IF      (SPA-SYONIKA        =   1   )  AND
                   (SPA-NAI-NENREI-YY  <   3   )
               MOVE    1                   TO  LNK-ORCSC-SYONIKA-ARI2
           END-IF
      *    ２４時間連携体制加算は自動発生しない
           MOVE    1                   TO  LNK-ORCSC-NETAKIRI-ARI3
      *
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
           MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-ORCSC-HAIKIKBN-FLG
      *    労災レセ電対応あり
           MOVE    SPA-ROURECEKBN      TO  LNK-ORCSC-ROURECEKBN
      *
      *
           EVALUATE    WRK-COM-SRYKBN (01)
      *            診療
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
      *H26.12
                   MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *            妥結率以下の設定
                   IF     (SPA-SRYYMD          >=  "20150101")
                       MOVE    SPA-DAKT50-KBN      TO  SPA-WORK(1:1)
                   END-IF
      *H30.4
      *            妊婦の設定
                   IF     (SPA-SRYYMD          >=  "20180401")
                       MOVE    FLG-NINPUKBN-IN     TO  SPA-WORK(2:1)
                   END-IF
                   CALL    "ORCSC10"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *H26.12
                   MOVE    WRK-SPA-WORK        TO  SPA-WORK
      *
                   ADD     1                   TO  SPA-SRYKBN-CNT (01)
      *            特定薬剤治療管理加算の初回算定日
                   MOVE    LNK-ORCSC-TOKTEI-YMD    TO  SPA-TOKTEI-YMD
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "24"
               WHEN    "25"
               WHEN    "26"
               WHEN    "27"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
      *            注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   CALL    "ORCSC30"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (03)
      *            処置
               WHEN    "40"
                   CALL    "ORCSC40"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (04)
      *            手術
               WHEN    "50"
                   CALL    "ORCSC50"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (05)
      *            麻酔
               WHEN    "54"
                   CALL    "ORCSC54"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (06)
      *            検査
               WHEN    "60"
               WHEN    "64"
                   CALL    "ORCSC60"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (07)
      *            画像診断
               WHEN    "70"
                   CALL    "ORCSC70"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (08)
      *            その他
               WHEN    "80"
      *          処方せん料は投薬とする
                 IF      WRK-COM-SRYSYUKBN (01)  =   "820"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
                 ELSE
                   CALL    "ORCSC80"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (09)
                 END-IF
      *            コメント等
      *********WHEN    "90"
               WHEN    "93"
               WHEN    "99"
                   PERFORM 510211-SRYKBN-90-SEC
      *            自費
               WHEN    "95"
               WHEN    "96"
                   CALL    "ORCSCJIH"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *************PERFORM 510212-SRYKBN-95-SEC
      *            入院（食事）
               WHEN    "90"
               WHEN    "92"
               WHEN    "97"
                   IF      SPA-NYUGAIKBN       =   1
                       CALL    "ORCSCN97"      USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
                       ADD     1               TO  SPA-SRYKBN-CNT (10)
                   ELSE
                       MOVE    "9020"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (01)
                   END-IF
           END-EVALUATE
      *
      *@
      *    投薬附加情報チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-JIDCD           =   SPACE )
      *        在宅・投薬・注射のみ
      ****     IF     (WRK-COM-SRYKBN (01)     =   "14"
      *                                        OR  "21"
      *                                        OR  "22"
      *                                        OR  "23"
      *                                        OR  "31"
      *                                        OR  "32"
      ***                                      OR  "33" )
                   PERFORM 51022-TENSUPLUS2-CHK-SEC
      *****    END-IF
           END-IF
      *****MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      **** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
      *H28.5
      *    一般名記載
           MOVE    IDX                 TO WRK-MAX
           IF     (WRK-COM-SRYKBN (01) =   "21"
                                       OR  "22"
                                       OR  "23"
                                       OR  "14" )
             AND  (SPA-SRYYMD          >=  "20160401")
             AND  (SPA-GENERICFLG      =   "1"   )
      *        一般名記載
               PERFORM 51023-MEIHENKAN-SEC
           END-IF
      *!!!!
      *
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        診区
               IF      IDX-Z               =   1
                   MOVE    WRK-COM-SRYKBN (IDX-Z)
                                           TO  WRK-GMN-SRYKBN(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-GMN-SRYKBN(IDX-Z)
                   MOVE    WRK-COM-SRYSYUKBN (01)
                                           TO  WRK-COM-SRYSYUKBN(IDX-Z)
               END-IF
      *        名称（最初の行に’＊’）
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "8" )  OR
                      (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "008")
                   CONTINUE
               ELSE
                   IF      IDX-Z               =   1
                       MOVE    "* "            TO  WRK-GMN-MEIH (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-GMN-MEIH (IDX-Z)
                   END-IF
               END-IF
      *        約束セット行の編集
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    LNK-ORCSC-ZAITEN        TO  WRK-COM-TENSU
                                                                (IDX-Z)
                   MOVE    LNK-ORCSC-ZAIKEI        TO  WRK-COM-KEI
                                                               (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU
                                                               (IDX-Z)
               END-IF
      *
      *        最終行を剤終了とする
               IF      (IDX-Z              =   SPA-MAX-LINE )  OR
                       (WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE )
                   MOVE    "1"             TO  WRK-COM-ZAIEND (IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND (IDX-Z)
               END-IF
      *        画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "2"                 TO  LNK-SC94-KBN
               MOVE    "J04"               TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HZN-COM-TBLREC
                                                       (WRK-HOZ-IDX)
               MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HZN-GMN-TBLREC
                                                       (WRK-HOZ-IDX)
      *
           END-PERFORM
           .
       5102-ZAIBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期投薬期間、投与量チェック処理
      *****************************************************************
       51022-TENSUPLUS2-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG2
           INITIALIZE                  ORCSC98AREA
           MOVE    "J04"               TO  LNK-SC98-PG
           MOVE    SPA-NAI-NENREI-YY   TO  LNK-SC98-NENREI-YY
           MOVE    SPA-NAI-NENREI-MM   TO  LNK-SC98-NENREI-MM
           MOVE    SPA-NAI-NENREI-DD   TO  LNK-SC98-NENREI-DD
      *H28.11
      *    薬剤投与量チェック区分
           MOVE    SPA-TOUYOCHK-FLG    TO  LNK-SC98-TOUYOCHK-FLG
           CALL    "ORCSC98"           USING
                                       SPA-AREA
                                       ORCSC98AREA
                                       WRK-SCR-REC
           IF      LNK-SC98-ERRMSG     NOT =   SPACE
               MOVE    LNK-SC98-ERRMSG     TO  SPA-ERRMSG2
           END-IF
           .
       51022-TENSUPLUS2-CHK-EXT.
           EXIT.
      *H28.4
      *****************************************************************
      *    投薬の名称再編集処理
      *****************************************************************
       51023-MEIHENKAN-SEC          SECTION.
      *
           IF     (WRK-COM-INGAITEN (WRK-MAX)  NOT =   ZERO )
              OR  (WRK-COM-SRYSYUKBN (01)  =   "148"
                                           OR  "149"
                                           OR  "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "298"
                                           OR  "295" )
      *        院外投薬
               PERFORM 510231-IPN-HEN-SEC
           ELSE
      *        院内
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                       UNTIL       IDX-Z       >   WRK-MAX
      *
                   IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"  )
                       MOVE    WRK-COM-MEI-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                       MOVE    SPACE       TO  WRK-COM-MEI-KBN(IDX-Z)
                   END-IF
               END-PERFORM
           END-IF
           .
       51023-MEIHENKAN-EXT.
           EXIT.
      *****************************************************************
      *    投薬の名称再編集処理
      *****************************************************************
       510231-IPN-HEN-SEC          SECTION.
      *
           INITIALIZE                  ORCSCGENNAMEAREA
           MOVE    SPACE               TO  ORCSCGENN-SYORIKBN
           MOVE    "1"                 TO  ORCSCGENN-SYORIKBN2
           MOVE    SPA-SRYYMD          TO  ORCSCGENN-SRYYMD
      *    後発医薬品への変更可署名(SYS-1030-KOUHATUKA)
           MOVE    SPA-KOUHATUKA       TO  ORCSCGENN-1030-KOUHATUKA
      *    後発品変更不可（処方毎）有
           IF      SPA-GENEFUKAFLG     =   "1"
               MOVE    "1"             TO  ORCSCGENN-KOUHATU-IN 
           END-IF
      *    後発品変更可（処方毎）有
           IF      SPA-GENEFUKAFLG     =   "2"
               MOVE    "2"             TO  ORCSCGENN-KOUHATU-IN 
           END-IF
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z   >   WRK-MAX      )
               MOVE    WRK-GMN-INPUTCD(IDX-Z)
                                       TO  ORCSCGENN-INPUTCD(IDX-Z)
               MOVE    WRK-COM-INPUTCD(IDX-Z)
                                       TO  ORCSCGENN-SRYCD (IDX-Z)
               MOVE    WRK-COM-KOUHATUKBN (IDX-Z)
                                       TO  ORCSCGENN-TNS-KOUHATUKBN
                                                            (IDX-Z) 
               MOVE    WRK-COM-TNSP-KISAIKBN (IDX-Z)
                                       TO  ORCSCGENN-IPN-KISAIKBN
                                                            (IDX-Z)
               MOVE    WRK-COM-YAKKAKJNCD (IDX-Z)
                                       TO  ORCSGETGE-YAKKAKJNCD
                                                            (IDX-Z)
               MOVE    WRK-COM-IPN-NAME(IDX-Z)
                                       TO  ORCSCGENN-HEN-NAME
                                                            (IDX-Z)
      *
               MOVE    IDX-Z           TO  ORCSCGENN-TBLMAX
           END-PERFORM
      *
           CALL    "ORCSCGENNAME"      USING    ORCSCGENNAMEAREA
                                                SPA-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z   >   WRK-MAX      )
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"  )
                   IF     (ORCSCGENN-NAME-KBN (IDX-Z)  =   "1"
                                                       OR  "3"  )
                      AND (ORCSCGENN-HEN-NAME(IDX-Z) NOT =   SPACE)
      *                一般名
                       MOVE    ORCSCGENN-HEN-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                       MOVE    "1"         TO  WRK-COM-MEI-KBN(IDX-Z)
                   ELSE
      *                銘柄名
                       MOVE    WRK-COM-MEI-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                       MOVE    SPACE       TO  WRK-COM-MEI-KBN(IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       510231-IPN-HEN-EXT.
           EXIT.
      *H28.4
      *
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF     (SPA-COM-SRYKBN  (IDX-Z)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32" OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-Z)(1:1)    =  "6" )
      *            禁忌薬剤チェック処理
                   IF      (SPA-COM-KNKFLG (IDX-Z)  =   SPACE )  AND
                           (SPA-COM-KNKUMU (IDX-Z)  =   SPACE )
                       PERFORM 510222-KNKYAK-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       51010-KNKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       510222-KNKYAK-CHK-SEC           SECTION.
      *    チェックレコードより、禁忌有無のチェックを行う
           INITIALIZE                  CHK-REC
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
           MOVE    SPA-J02-SRYYMD      TO  CHK-YUKOSTYMD
           MOVE    SPA-J02-SRYYMD      TO  CHK-YUKOEDYMD
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
      *
           IF      FLG-CHKMST          =   1
               MOVE    "1"             TO  SPA-COM-KNKUMU (IDX-Z)
           END-IF
      *
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K
           INITIALIZE                  WRK-KNKCHK-AREA
           PERFORM
                   UNTIL      FLG-CHKMST        =   1
      *******  PERFORM VARYING     IDX-C       FROM    1   BY  1
      *                UNTIL      (IDX-C           >   10  )  OR
      *****                       (CHK-CD (IDX-C)  =   SPACE )
                   PERFORM 5102221-KNKCHK-SYORI-SEC
      *****    END-PERFORM
      *
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-K               >   ZERO
               MOVE    "1"             TO  SPA-COM-KNKFLG (IDX-Z)
               MOVE    SPA-GMN-MEISYO (IDX-Z)  TO  WRK-KNK-TOUMEI (01)
      *        同一診療コードをなくす
               PERFORM VARYING     IDX-C   FROM    1   BY  1
                       UNTIL       IDX-C   >   30
                   PERFORM VARYING     IDX-X   FROM    IDX-C   BY  1
                           UNTIL       IDX-X   >   30
                       IF     (IDX-C       NOT =   IDX-X   )   AND
                              (WRK-KNK-SRYCD (1 IDX-C)
                                               =   WRK-KNK-SRYCD
                                                             (1 IDX-X))
                           MOVE    SPACE       TO  WRK-KNK-TBL2
                                                             (1 IDX-X)
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
      *        初期画面の時、エラー画面なし
               IF      FLG-TOROKU          =   "2"
                   CONTINUE
               ELSE
                   PERFORM 510229-POP-ERR-SEC
               END-IF
           END-IF
           .
       510222-KNKYAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       5102221-KNKCHK-SYORI-SEC           SECTION.
      *
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   100 )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
               IF     (SPA-KNK-SRYCD (IDX-S)   =   CHK-CD   ) AND
                      (SPA-KNK-TOGETU (IDX-S)  =   "1"  ) 
                   ADD     1                   TO  IDX-K
                   MOVE    SPA-KNK-SRYMEI (IDX-S)  TO  WRK-KNK-KNKMEI
                                                              (1 IDX-K)
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  WRK-KNK-SRYCD
                                                              (1 IDX-K)
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   SPA-MAX-LINE ) OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE )
               IF     (SPA-COM-SRYKBN  (IDX-S)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32" OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-S)     =   CHK-CD  )
                 AND  (IDX-S                   NOT =   IDX-Z          )
                   ADD     1                   TO  IDX-K
                   MOVE    SPA-GMN-MEISYO (IDX-S)  TO  WRK-KNK-KNKMEI
                                                              (1 IDX-K)
                   MOVE    SPA-GMN-INPUTCD (IDX-S) TO  WRK-KNK-SRYCD
                                                              (1 IDX-K)
               END-IF
           END-PERFORM
           .
       5102221-KNKCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     コメント等　編集処理
      *****************************************************************
       510211-SRYKBN-90-SEC            SECTION.
      *
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF      WRK-COM-KISOTEN (IDX-Z) =   ZERO
                   MOVE    ZERO            TO  WRK-COM-KEI (IDX-Z)
                   MOVE    1               TO  WRK-COM-KAISU(IDX-Z)
                   MOVE    1               TO  WRK-COM-SURYO(IDX-Z)
                   MOVE    ZERO            TO  WRK-COM-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-SURYO(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-KAISU(IDX-Z)
                   MOVE    ZERO            TO  WRK-GMN-KEI (IDX-Z)
               END-IF
           END-PERFORM
      *
      *
           .
       510211-SRYKBN-90-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      IDX             =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-COM-TBLREC (IDY) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    WRK-GMN-TBLREC (IDY) 
                                           TO  SPA-GMN-TBLREC(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   MAX-GMN     +    1
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41014-GYOINSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       41015-ZAIDEL-SEC              SECTION.
      *
           IF      (SPA-GMN-IDX        =   1   )  OR
                   (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
                                       =   "1" )
               CONTINUE
           ELSE
               MOVE    "0023"              TO  SPA-ERRCD
               GO      TO      41015-ZAIDEL-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM 510-TBLHEN-SEC
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX   +   1
      *    減がある時、その次より
           IF      SPA-COM-INPUTCD (IDX2)  =   "820000047"
               ADD     1                   TO  IDX2
           END-IF
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX) 
                                           TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX) 
                                           TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
               END-IF
           END-PERFORM
      *
           .
       41016-SETDEL-EXT.
           EXIT.
      *****************************************************************
      *    １行前へカーソル移動処理
      *****************************************************************
       41017-ZENGYOU-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-MAE
           IF      SPA-GMN-IDX         =   1
               GO      TO      41017-ZENGYOU-SET-EXT
           END-IF
      *    次の行がある時、削除行とする
           COMPUTE IDX                 =   SPA-GMN-IDX    +   1
           PERFORM VARYING     WRK-IDX     FROM    IDX    BY   1
                   UNTIL      (WRK-IDX     >   SPA-MAX-LINE )  OR
                              (FLG-MAE     =   1   )
               IF     (SPA-GMN-INPUTCD (WRK-IDX)   NOT =   SPACE )  AND
                      (SPA-COM-SET     (WRK-IDX)       =   SPACE )
                   MOVE    1               TO  FLG-MAE
               END-IF
           END-PERFORM
           IF      FLG-MAE             =    1
               MOVE    ZERO            TO   FLG-MAE
               GO      TO      41017-ZENGYOU-SET-EXT
           END-IF
      *    カーソル位置決定
           COMPUTE IDX                 =   SPA-GMN-IDX    -   1
           MOVE    ZERO                TO  WRK-IN-IDX
           PERFORM VARYING     WRK-IDX     FROM    IDX  BY  -1
                   UNTIL      (WRK-IDX     <   1   )  OR
                              (WRK-IN-IDX  >   ZERO)
               IF     (SPA-GMN-INPUTCD (WRK-IDX)   NOT =   SPACE )  AND
                      (SPA-COM-SET     (WRK-IDX)       =   SPACE )
                   MOVE    WRK-IDX             TO  WRK-IN-IDX
               END-IF
           END-PERFORM
           IF      WRK-IN-IDX          =   ZERO
               GO      TO      41017-ZENGYOU-SET-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-MAE
      *
           IF      SPA-MAP-IDX         =   1
               COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE
                                           -   1
           END-IF
      *
           MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           MOVE    "1"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
           MOVE    1                   TO  FLG-MAE
      *
           .
       41017-ZENGYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
      *            削除行が加算料を自動追加した時、自動追加分も削除
                   IF     (WRK-COM-INPUTCD(IDX)    NOT =   SPACE )  AND
                          (IDX                     <   SPA-MAX-LINE) AND
                          (WRK-COM-AUTOKBN  (IDX + 1)  =   "1"   )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX + 1)
                   END-IF
      *            器材商品名は次も削除
                   IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "058") AND
                          (WRK-COM-AUTOKBN2 (IDX + 1)  =   "1"   )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX + 1)
                   END-IF
      *            前が器材商品名
                   IF     (WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE )
                      AND (WRK-COM-INPUTCD(IDX - 1)(1:3)
                                                       =   "058")
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX - 1)
                   END-IF
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX) 
                                           TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX) 
                                           TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   MAX-GMN    +    1
           END-IF
      *
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    各診療行為チェックへ
      *****************************************************************
       520-ALL-CHEK-SEC                SECTION.
      *
      *****MOVE    SPA-MAP-IDX         TO  LNK-ORCSC-MAP-IDX
      *****MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-ROUJIN          TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NAI-NENREI      TO  LNK-ORCSC-NENREI 
           MOVE    SPA-NAI-INGAIKBN    TO  LNK-ORCSC-INGAIKBN
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *D   MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO LNK-ORCSC-TOKTEI-YMD
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *    小児科外来診療科があり、３歳満の時
           IF      (SPA-SYONIKA        =   1   )  AND
                   (SPA-NAI-NENREI-YY  <   3   )
               MOVE    1                   TO  LNK-ORCSC-SYONIKA-ARI2
           END-IF
      *    ２４時間連携体制加算は自動発生しない
           MOVE    1                   TO  LNK-ORCSC-NETAKIRI-ARI3
      *    労災レセ電対応あり
           MOVE    SPA-ROURECEKBN      TO  LNK-ORCSC-ROURECEKBN
      *
      *            診療
           IF      SPA-SRYKBN-CNT (01) >   ZERO
               CALL    "ORCSC10"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            投薬
           IF      SPA-SRYKBN-CNT (02) >   ZERO
               CALL    "ORCSC20"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            注射
           IF      SPA-SRYKBN-CNT (03) >   ZERO
               CALL    "ORCSC30"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            処置
           IF      SPA-SRYKBN-CNT (04) >   ZERO
               CALL    "ORCSC40"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            手術
           IF      SPA-SRYKBN-CNT (05) >   ZERO
               CALL    "ORCSC50"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            麻酔
           IF      SPA-SRYKBN-CNT (06) >   ZERO
               CALL    "ORCSC54"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            検査
           IF      SPA-SRYKBN-CNT (07) >   ZERO
               CALL    "ORCSC60"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            画像診断
           IF      SPA-SRYKBN-CNT (08) >   ZERO
               CALL    "ORCSC70"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            その他
           IF      SPA-SRYKBN-CNT (09) >   ZERO
               CALL    "ORCSC80"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *        入院（食事）
           IF      SPA-SRYKBN-CNT (10) >   ZERO
               CALL    "ORCSCN97"      USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
      *
           .
       520-ALL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    1                   TO  FLG-END2
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *****MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEAR-SEC        SECTION.
      *
           IF      SPA-J04-SYORI       =   1
               INITIALIZE                  SPA-SCR-REC
               MOVE    SPACE               TO  SPA-J04-SRYKBN
               MOVE    ZERO                TO  SPA-GMN-MAX
               MOVE    ZERO                TO  IDX2
               MOVE    ZERO                TO  SPA-CHOKITOFLG
           END-IF
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    行為セット　処理
      *****************************************************************
       420-KOUISET-SEC             SECTION.
      *
           .
       420-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号検索　処理
      *****************************************************************
       430-BANKENSAKU-SEC             SECTION.
      *
           .
       430-BANKENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      450-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
      *
           MOVE    1                   TO  SPA-MAP-IDX
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  MAX-GMN
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       41001-KOUI-ALLCHK-SEC       SECTION.
      *
      *    画面をＳＰＡへ編集
           MOVE    1                   TO  WRK-IDX2
           PERFORM 2002-GMNSPA-SET-SEC
      *
      *    入力コード・名称チェック処理
           MOVE    "1"                 TO  FLG-TOROKU
           MOVE    1                   TO  WRK-STR
           PERFORM 410112-KOUI-SPACHK-SEC
           .
       41001-KOUI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      **** 更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      4100-TOROKU-EXT
      *****END-IF
      *
      *H24.5 すべてのチェック後へ
      **** 禁忌更新前のチェック
      *    PERFORM 41001-YAKKNK-CHK-SEC
      *
      *     禁忌エラーの時、エラー表示へ
      *    IF      WRK-KNKCHK-AREA     NOT =   SPACE
      *        MOVE    "J03"               TO  SPA-J041-SAKI
      *        PERFORM 410019-POP-J041-SEC
      *        GO      TO      4100-TOROKU-EXT
      **** END-IF
      *
      *    剤のチェック
           PERFORM 45011-NAIYOU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4100-TOROKU-EXT
           END-IF
      *    同一剤が存在している時、エラーとする
           IF     (FLG-OK              =   1   )  AND
                  (FLG-OK2             =   ZERO  OR  1  )
               MOVE    "1025"              TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
           IF      SPA-NEIGEN-KBN      =   1
               IF      SPA-NEIGEN-FLG      =   ZERO
                   MOVE    "K102"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-NEIGEN-FLG
                   GO      TO      4100-TOROKU-EXT
               END-IF
           END-IF
      *
      ***  IF     (FLG-OK2             =   1   )  AND
      *           (WRK-ZAINUM          =   ZERO )
      *        剤変更なしとして、処理を終了する
      *        PERFORM 210-BACK
      *        GO      TO      4100-TOROKU-EXT
      ***  END-IF
      *
      *    外用薬で回数が２８以上の時、警告表示
           IF      SPA-CHOKITOFLG      =   1
               IF      SPA-JID1-FLG        =   SPACE
                   MOVE    "0102"          TO  SPA-JIDCD
                   GO      TO      4100-TOROKU-EXT
               END-IF
           END-IF
      *
      *    禁忌更新前のチェック
           PERFORM 41001-YAKKNK-CHK-SEC
      *     禁忌エラーの時、エラー表示へ
           IF      WRK-KNKCHK-AREA     NOT =   SPACE
               MOVE    "J03"               TO  SPA-J041-SAKI
               PERFORM 410019-POP-J041-SEC
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-J99-DATA
      *
           IF      SPA-J04-SYORI       =   ZERO
               MOVE    "0101"              TO  SPA-JIDCD
           ELSE
               MOVE    "0103"              TO  SPA-JIDCD
           END-IF
      *
      *    更新処理
      *****PERFORM 41002-KOUSIN-SYORI-SEC
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    診療行為マスタ更新
           PERFORM 4501-SINKOUI-KOU-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1                   TO  FLG-END2
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
           IF     (SPA-SEL-RRKYMD      =   SPACE )
             AND  (SPA-J04-SYORI       =   ZERO  )
             AND  (SPA-SRYKBN          =   SPA-J04-SRYKBN)
      *        診療会計マスタ更新
               PERFORM 4502-SINKAI-KOU-SEC
           ELSE
      *        診療会計マスタ追加、受診履歴変更
               PERFORM 4504-SRYACCT-ADD-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1                   TO  FLG-END2
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *    主科設定
           PERFORM 410029-ORCSSYUKAMAIN-SEC
      *
      *    内分泌負荷試験判定
           IF      FLG-NAIBUN          =   1
      *        内分泌負荷試験包括点数チェック
               INITIALIZE                  ORCSSANFUKAAREA
               INITIALIZE                  SPA-SCR-REC
               MOVE    3                   TO  LNK-SANFUKA-SYORI
               MOVE    "J04"               TO  LNK-SANFUKA-PG
               IF      SPA-SEL-RRKYMD      =   SPACE
                   MOVE    SPA-J02-SRYYMD(1:6) TO  LNK-SANFUKA-SRYYMD
                   MOVE    "01"                TO  LNK-SANFUKA-SRYYMD
                                                             (7:2)
               ELSE
                   MOVE    SPA-SEL-RRKYMD      TO  LNK-SANFUKA-SRYYMD
               END-IF
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
               IF      LNK-SANFUKA-ERRMSG  NOT =   SPACE
                   MOVE    "K001"              TO  SPA-ERRCD
      *            警告済み
                   MOVE    1                    TO  SPA-KEI-FLG
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        終了
               MOVE    WRK-ZAINUM          TO  SPA-MAE-ZAINUM
               PERFORM 410021-END-SYORI-SEC
           ELSE
               MOVE    1                   TO  FLG-END2
           END-IF
      *
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       410029-ORCSSYUKAMAIN-SEC             SECTION.
      *
      *    包括まとめ入力以外
           IF      SPA-HKNCOMBINUM     =   "9999"
      *      包括診療対象外
             OR   (ACCT-JIHOKBN        =   "1"   )
               CONTINUE
           ELSE
               INITIALIZE                      ORCSSYUACCAREA
               MOVE    "03"                TO  LNK-SYUACC-I-KBN
      *        診療年月
               MOVE    SPA-J02-SRYYMD(1:6) TO  LNK-SYUACC-I-SRYYM
               CALL    "ORCSSYUKAMAIN"     USING
                                           SPA-AREA
                                           ORCSSYUACCAREA
           END-IF
           .
      *
       410029-ORCSSYUKAMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       410021-END-SYORI-SEC            SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    1                   TO  FLG-END2
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
      *
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410021-END-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ更新処理
      *****************************************************************
       4501-SINKOUI-KOU-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-NAIBUN
      *
      *    算定履歴の削除
           PERFORM 45013-SANTEI-DELETE-SEC
      *
           IF      SPA-SEL-RRKYMD      =   SPACE
             AND   SPA-J04-SYORI       =   ZERO
      *        受診日選択なしの時、対象の剤をクリアする
               PERFORM 45012-DBDELETE-SEC
               MOVE    SPA-ZAINUM          TO  WRK-ZAINUM
           ELSE
      *        剤番号取得処理
               PERFORM 45013-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4501-SINKOUI-KOU-EXT
               END-IF
               INITIALIZE                      TBL-SRYACT-AREA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KAISUKEI
           MOVE    ZERO                TO  WRK-KINGAK
           MOVE    ZERO                TO  WRK-MEINUM
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYCDKEI
           MOVE    ZERO                TO  WRK-SURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           INITIALIZE                      WRK-WKSRY-AREA
      *    附加情報
           INITIALIZE                      WKSRYACTPLUS-REC
      *
      *    診療行為新規作成
           MOVE    ZERO                TO  IDS
           MOVE    ZERO                TO  WRK-RENNUM
           INITIALIZE                  SRYACT-REC
      *
           MOVE    ZERO                TO  FLG-HENKOU
           MOVE    ZERO                TO  IDX-SR
      *
           MOVE    ZERO                TO  FLG-GEN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "."
                   CONTINUE
               ELSE
      *            減がある時、その次より
                  IF     (SPA-NEIGEN-KBN           =   1  )  AND
                         (SPA-COM-INPUTCD (IDX)    =   "820000047" )
                       MOVE    1                   TO  FLG-GEN
                   END-IF
                   ADD     1               TO  IDS
      *            診療行為内容編集
                   PERFORM 45101-SINKOUI-HEN-SEC
      *
                   IF      IDS             >=  5
      *                新規レコード追加
                       PERFORM 45102-SINKOU-INS-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDS                 >   ZERO
      *        新規レコード追加
               PERFORM 45102-SINKOU-INS-SEC
           END-IF
      *
           .
       4501-SINKOUI-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤のチェック処理
      *****************************************************************
       45011-NAIYOU-CHK-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KAISUKEI
           MOVE    ZERO                TO  WRK-KINGAK
           MOVE    ZERO                TO  WRK-MEINUM
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-ZAIKBN
           MOVE    ZERO                TO  WRK-SRYCDKEI
           MOVE    ZERO                TO  WRK-SURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           INITIALIZE                      WRK-WKSRY-AREA
      *    附加情報
           INITIALIZE                      WKSRYACTPLUS-REC
           MOVE    ZERO                TO  IDW
           MOVE    ZERO                TO  FLG-NAIBUN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
             IF    SPA-GMN-INPUTCD(IDX)(1:1)   =   "." 
               CONTINUE
             ELSE
      *
      *        自費
      *********IF      SPA-COM-SRYKBN (IDX)    =   "95" OR "96"
               IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
      *************ADD     SPA-COM-KEI  (IDX)  TO  WRK-KINGAK
                   ADD     SPA-COM-JIHIMONEY  (IDX)  TO  WRK-KINGAK
               END-IF
      *
      *        画面の計を集計する（１剤で１件）
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
                   IF     (SPA-COM-SRYKBN (IDX)    =   "95" OR "96" )
                   AND    (SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2" )
                       MOVE    WRK-KINGAK              TO  WRK-ZAITEN
                   ELSE
                       MOVE    SPA-COM-TENSU  (IDX)    TO  WRK-ZAITEN
                   END-IF
                   MOVE    SPA-COM-SYUTEN1(IDX)    TO  WRK-SYUTEN1
                   MOVE    SPA-COM-SYUTEN2 (IDX)   TO  WRK-SYUTEN2
                   MOVE    SPA-COM-YKZTEN  (IDX)   TO  WRK-YKZTEN
                   MOVE    SPA-COM-KIZAITEN(IDX)   TO  WRK-KIZAITEN
      *
                   IF      SPA-COM-YAKUHYO (IDX)   =   "1"
                       MOVE    1                   TO  WRK-ZAIKBN
                   END-IF
                   IF      SPA-COM-GENTEN  (IDX)   =   "1"
                       MOVE    2                   TO  WRK-ZAIKBN
                   END-IF
      *
      *H28.9
      *            持参薬
                   IF      SPA-COM-JISANYAKU(IDX)  =   "1"
                       MOVE    3                   TO  WRK-ZAIKBN
                   END-IF
                   IF      SPA-COM-JISANYAKU(IDX)  =   "2"
                       MOVE    4                   TO  WRK-ZAIKBN
                   END-IF
               END-IF
      *
      *        各附加情報
               PERFORM 450112-WKSRYACTPLUS-SEC
      *        約束セット
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
                   CONTINUE
               ELSE
      *            診療コード計
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WRK-SRYCD-9
                   ADD     WRK-SRYCD-9             TO  WRK-SRYCDKEI
      *            数量
                   ADD     SPA-COM-SURYO  (IDX)    TO  WRK-SURYOKEI
               END-IF
      *        明細数
               ADD     1                       TO  WRK-MEISAISU
      *!!
      *        出来高診療
               IF      SPA-COM-INPUTCD (IDX)   =   "099999903"
                   MOVE    SPA-GMN-HKTKBN-LST (1)
                                               TO  SPA-GMN-HKTKBN-G
               END-IF
      *!!
             END-IF
      *
           END-PERFORM
      *
      *    剤点数がゼロの時、自費を点数へ
      ***  IF      WRK-ZAITEN          =   ZERO
           IF    ( SPA-J04-SRYKBN      =   "95"
                                       OR  "96" )
               MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
           END-IF
      *
      *    包括診療編集
           IF      SPA-GMN-HKTKBN          =   "1"
               MOVE   "1"                  TO  SPA-J04-JIHOKBN
           ELSE
               MOVE   SPACE                TO  SPA-J04-JIHOKBN
           END-IF
      *H28.9
      *    持参薬は包括診療へ
           IF      WRK-ZAIKBN          =   3
                                       OR  4
               MOVE   "1"                  TO  SPA-J04-JIHOKBN
           END-IF
      *
      *    診療会計剤チェックサブ
           INITIALIZE                      ORCSCZAICHKAREA
           MOVE    "1"                     TO  ORCSCZAICHK-KBN
           MOVE    SPA-J01-SRYKA           TO  ORCSCZAICHK-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6)
                                           TO  ORCSCZAICHK-SRYYM
           MOVE    SPA-J04-SRYKBN          TO  ORCSCZAICHK-SRYKBN
           MOVE    SPA-COM-SRYSYUKBN(01)   TO  ORCSCZAICHK-SRYSYUKBN
           MOVE    SPA-HKNCOMBINUM         TO  ORCSCZAICHK-HKNCOMBI
           MOVE    WRK-SRYCDKEI            TO  ORCSCZAICHK-SRYCDTOTAL
           MOVE    WRK-SURYOKEI            TO  ORCSCZAICHK-SURYOUTOTAL
           MOVE    WRK-ZAITEN              TO  ORCSCZAICHK-ZAITEN
           MOVE    WRK-MEISAISU            TO  ORCSCZAICHK-MEISAISU
           MOVE    1                       TO  ORCSCZAICHK-ZAIKAISU
           MOVE    SPA-J04-JIHOKBN         TO  ORCSCZAICHK-JIHOKBN
           MOVE    ZERO                TO  IDX-ATO
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX1)(1:1) NOT =   "."
                   ADD     1                   TO  IDX-ATO
                   MOVE    SPA-COM-INPUTCD (IDX1)
                                               TO  ORCSCZAICHK-SRYCD
                                                             (IDX-ATO)
                   MOVE    SPA-COM-SURYO   (IDX1)
                                               TO  ORCSCZAICHK-SRYSURYO
                                                             (IDX-ATO)
                   IF     (SPA-COM-DATAKBN (IDX1)   =   "3" )
      *                画像診断のみ
                    AND   (SPA-COM-SRYKBN  (IDX1)  =   "70" )
                       MOVE    SPA-COM-BUNGSU (IDX1)
                                               TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX-ATO)
                   END-IF
      *            外用薬の入力回数
                   IF      SPA-COM-SRYKBN  (IDX1)   =   "23"
                       MOVE    SPA-CHOKIKAISU
                                               TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX-ATO)
                   END-IF
      *            入力コメント
                   IF      SPA-COM-MEIFLG (IDX1)    =   "1"
                       IF      SPA-COM-INPUTCD(IDX1)  =   "810000001"
                                                       OR  "008500000"
                                                       OR  "008600000"
                           MOVE    SPA-GMN-MEISYO-G (IDX1)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                       ELSE
                           MOVE    SPA-GMN-MEISYO   (IDX1)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                       END-IF
                   ELSE
                       IF     (SPA-COM-INPUTCD(IDX1)(1:2) =   "83"
                                                          OR  "84")
      *R02.4
                          OR  (SPA-COM-INPUTCD(IDX1)(1:2) =   "85")
                          OR  (SPA-COM-INPUTCD(IDX1)(1:4) =   "0083"
                                                          OR  "0084")
                           MOVE    SPA-GMN-MEISYO(IDX1)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                       END-IF
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX1)(1:1)  =   "8"   ) OR
                          (SPA-COM-INPUTCD(IDX1)(1:4)  =   "0084")
      *             用法コメント
                    OR   ((SPA-COM-INPUTCD(IDX1)(1:3)  =   "001" )  AND
                          (SPA-COM-YCOMKBN(IDX1)       =   1     ))
      *H30.4
                    OR    (SPA-COM-INPUTCD(IDX1)(1:7)  =   "0992081")
                       MOVE    SPA-COM-INPUTCHI-G(IDX1)
                                           TO  ORCSCZAICHK-INPUTCHI-G
                                                             (IDX-ATO)
                   END-IF
      *            自費金額
                   IF      SPA-COM-KEIFLG (IDX1)   =   "1"  OR  "2"
                       MOVE    SPA-COM-JIHIMONEY(IDX1)
                                               TO  ORCSCZAICHK-JIHIMONEY
                                                             (IDX-ATO)
                   END-IF
               END-IF
           END-PERFORM
      *    明細がゼロの時、エラー
           IF      WRK-MEISAISU        =   ZERO
               MOVE    "1031"              TO  SPA-ERRCD
           ELSE
               CALL    "ORCSCZAICHK"       USING   SPA-AREA
                                                   ORCSCZAICHKAREA
               MOVE    ORCSCZAICHK-ZAINUM  TO  WRK-ZAINUM
      *
      *        同一剤なし
               IF      WRK-ZAINUM          =   ZERO
                   MOVE    ZERO            TO  FLG-OK
               ELSE
                   MOVE    1               TO  FLG-OK
                   IF      WRK-ZAINUM      =   SPA-ZAINUM
      *                変更なし
                       IF      SPA-SEL-RRKYMD      =   SPACE
                           MOVE    2               TO  FLG-OK2
                       ELSE
                           MOVE    1               TO  FLG-OK2
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       45011-NAIYOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       450112-WKSRYACTPLUS-SEC          SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
           IF     (SPA-HKNKBN          =   1   )  AND
                  (SPA-COM-SRYKBN (IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"
                                               OR  "09592"
                                               OR  "09593"
                                               OR  "09594")
               PERFORM 450112-JIBAISEKI-SET-SEC
           END-IF
      *
      *    労災保険は円と点数に分ける
           IF     (SPA-COM-TENSIKIBETU (IDX)   =   1  )  AND
                  (SPA-COM-RSIKBN      (IDX)   =   1  )  AND
                  (SPA-COM-INPUTCD (IDX)(1:1)  =   "1")
      *        円
      *H26.2
      *        施術加算対応
               PERFORM 450112-RSI-SKASAN-SEC
      *******  COMPUTE WRK-EN          =   SPA-COM-TEN (IDX)
               COMPUTE WRK-EN          =   WRK-ENTANKA
                                       *   SPA-COM-KAISU (IDX)
      ******** MOVE    SPA-COM-TEN (IDX)       TO  WRK-TANKA
               MOVE    WRK-ENTANKA     TO  WRK-TANKA
      *        労災の円（単価）
               MOVE    WRK-TANKA       TO  SPA-COM-RSIKIN(IDX)
           END-IF
      *    労災の円
           IF      SPA-HKNKBN          =   1
               IF      SPA-COM-RSIKIN(IDX) >   ZERO
                   MOVE    SPA-COM-RSIKIN(IDX) TO  WKSRYP-RSIKINGAKU
                   MOVE    "1"                 TO  WKSRYP-ADDKBN
               END-IF
           ELSE
               MOVE    ZERO                TO  WKSRYP-RSIKINGAKU
           END-IF
      *    点数区分
           IF     (SPA-COM-TENSIKIBETU(IDX)   =   7
                                               OR  8  )  AND
                  (SPA-COM-KISOTEN (IDX)  =   SPA-COM-TENSU
                                                 (SPA-GMN-MAX))
               MOVE    1                   TO  WKSRYP-PLUSKBN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    手技毎の内容（診察・手術・画像診断・処置）
           IF     (SPA-COM-SYUFLG(IDX)    =   1  )  AND
                  (SPA-COM-SYUTEN-M(IDX)  NOT =  SPA-COM-SYUTEN1
                                                       (SPA-GMN-MAX))
               MOVE    "1"                 TO  WKSRYP-ADDKBN
               ADD     1                   TO  IDW
      *        開始手技コード
               MOVE    SPA-COM-INPUTCD(IDX)   TO  WKSRYP-SRYCD (IDW)
      *        手技点数
               MOVE    SPA-COM-SYUTEN-M(IDX)  TO  WKSRYP-SYUTEN (IDW)
      *        薬剤点数
               MOVE    SPA-COM-YKZTEN-M(IDX)  TO  WKSRYP-YKZTEN (IDW)
      *        その他器材点数（フィルム・酸素・窒素以外）
               MOVE    SPA-COM-KIZTEN-M(IDX)  TO  WKSRYP-KIZAITEN (IDW)
      *        フィルム点数
               MOVE    SPA-COM-FIRTEN-M(IDX)  TO  WKSRYP-FIRTEN (IDW)
           ELSE
      *        フィルム点数のみ
               IF      SPA-COM-FIRTEN-M(IDX)  NOT =  ZERO
                   ADD     1                   TO  IDW
                   MOVE    "1"                 TO  WKSRYP-ADDKBN
      *            その他器材点数（フィルム・酸素・窒素以外）
                   MOVE    SPA-COM-KIZTEN-M(IDX)
                                               TO  WKSRYP-KIZAITEN (IDW)
                   MOVE    SPA-COM-FIRTEN-M(IDX)
                                               TO  WKSRYP-FIRTEN (IDW)
               END-IF
           END-IF
      *    酸素点数
           IF      SPA-COM-SANSOTEN(IDX)  NOT =   ZERO
               MOVE    SPA-COM-SANSOTEN(IDX)   TO  WKSRYP-SANSOTEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    窒素点数
           IF      SPA-COM-CHISOTEN(IDX)  NOT =   ZERO
               MOVE    SPA-COM-CHISOTEN(IDX)  TO  WKSRYP-CHISOTEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *ver.4.7
      *    院外処方点数
           IF      SPA-COM-INGAITEN(IDX)   NOT =   ZERO
               MOVE    SPA-COM-INGAITEN(IDX)   TO  WKSRYP-INGAITEN
               MOVE    "1"                     TO  WKSRYP-ADDKBN
           END-IF
      *
      *    内分泌負荷試験用
           IF     (SPA-COM-SRYKBN(IDX)    =   "60" )  AND
                  (SPA-COM-DATAKBN(IDX)   =   "1"   )  AND
                  (SPA-COM-HOUKNSKBN(IDX) =   08    )
      *        当月算定フラグ
               MOVE    1                   TO  WKSRYP-MSANTEITFLG
      *        当月算定点数区分
               IF      SPA-COM-TEN (IDX)  =   SPA-COM-KISOTEN(IDX)
                   MOVE    1               TO  WKSRYP-MSANTEITENKBN
               ELSE
                   MOVE    2               TO  WKSRYP-MSANTEITENKBN
               END-IF
      *        当月算定点数
               MOVE    SPA-COM-KISOTEN(IDX)   TO WKSRYP-MSANTEITEN
           END-IF
      *
           .
       450112-WKSRYACTPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責診断料・明細料その他の編集処理
      *****************************************************************
       450112-JIBAISEKI-SET-SEC        SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
           IF     (SPA-COM-SRYSYUKBN (IDX)
                                       =   "809"  )
      *        金額入力
               COMPUTE WRK-EN              =   SPA-COM-JIHIMONEY (IDX)
                                           *   SPA-COM-KAISU (IDX)
               MOVE    SPA-COM-JIHIMONEY (IDX) TO  WRK-TANKA
           ELSE
      *    労災保険−その他
               COMPUTE WRK-EN              =   SPA-COM-TEN (IDX)
                                           *   SPA-COM-KAISU (IDX)
               MOVE    SPA-COM-TEN (IDX)       TO  WRK-TANKA
           END-IF
      *    労災の円
           MOVE    WRK-TANKA           TO  SPA-COM-RSIKIN(IDX)
      *
           .
       450112-JIBAISEKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    施術加算対応処理
      *****************************************************************
       450112-RSI-SKASAN-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-ENTANKA
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
               COMPUTE IDR-1           =   SPA-GMN-MAX +   1
           ELSE
               COMPUTE IDR-1           =   IDX     +   1
           END-IF
           PERFORM VARYING     IDR-2   FROM    IDR-1   BY  1
                   UNTIL      (IDR-2   >   SPA-GMN-MAX  )
      *
               IF     (SPA-COM-TENSIKIBETU (IDR-2) =   "5"  )  AND
                      (SPA-COM-INPUTCD(IDR-2)(1:3) =   "101")
      *            ％加算　円未満切り捨て 
                   MOVE    ZERO                TO  WRK-TENKEISAN
                   COMPUTE WRK-TENKEISAN   =   SPA-COM-TEN (IDX)
                                           *   SPA-COM-KISOTEN (IDR-2)
                                           /   100
                   MOVE    WRK-TENKEISAN       TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *
               IF     (SPA-COM-TENSIKIBETU (IDR-2) =   "6"  )  AND
                      (SPA-COM-INPUTCD(IDR-2)(1:3) =   "101")
      *            ％減算　円未満切り捨て 
                   MOVE    ZERO                TO  WRK-TENKEISAN
                   COMPUTE WRK-TENKEISAN   =   SPA-COM-TEN (IDX)
                                           *   SPA-COM-KISOTEN (IDR-2)
                                           /   100
                   MOVE    WRK-TENKEISAN       TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
               IF      (SPA-COM-ZAIEND (IDR-2)  =   "1" )
                   MOVE    SPA-GMN-MAX     TO  IDR-2
               END-IF
           END-PERFORM
           COMPUTE WRK-ENTANKA         =   SPA-COM-TEN (IDX)
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
           .
       450112-RSI-SKASAN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    対象の剤クリア処理
      *****************************************************************
       45012-DBDELETE-SEC              SECTION.
      *
      *H27.10
      *    保存
           PERFORM 450121-SRYACT-HZN-SEC
      *
      *    患者コメントをクリアする
           INITIALIZE                  PTCOM-REC
           MOVE    SPA-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
           MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *D   IF      MCP-RC          NOT =   ZERO
      *D       DISPLAY "J04 PTCOM DEL ERR;"    MCP-RC
      *D   END-IF
      *
      *    診療行為データ削除
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACT DEL ERR;"    MCP-RC
           END-IF
      *
           .
       45012-DBDELETE-EXT.
           EXIT.
      *
      *H27.10
      *****************************************************************
      *   診療行為内容保存処理
      *****************************************************************
       450121-SRYACT-HZN-SEC          SECTION.
      *
           INITIALIZE                      TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-SR
      *
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM
               UNTIL    (FLG-SRYACT        =   1          )
                    OR  (IDX-SR            >=  10   )
               ADD     1                   TO  IDX-SR
               MOVE    SRYACT-REC          TO  TBL-SRYACT-REC(IDX-SR)
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       450121-SRYACT-HZN-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴削除処理
      *****************************************************************
       45013-SANTEI-DELETE-SEC          SECTION.
      *
      *    算定履歴削除
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
               PERFORM VARYING     IDZ    FROM    1   BY  1
                       UNTIL      (IDZ        >   5   )  OR
                                  (SRY-SRYCD(IDZ)     =   SPACE )
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1"  )
      *                システム予約で履歴があるもの
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 450122-SANTEI-DEL-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       45013-SANTEI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴削除処理
      *****************************************************************
       450122-SANTEI-DEL-SEC          SECTION.
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "3"                 TO  SSANTEI-SYORI
           MOVE    "J04"               TO  SSANTEI-PG
           IF      SPA-SEL-RRKYMD      =   SPACE
               MOVE    SRY-SRYYM           TO  SSANTEI-SRYYMD(1:6)
               MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
               PERFORM   VARYING   IDX-D   FROM    1   BY  1
                         UNTIL     IDX-D       >   31
                   IF      SPA-SEL-DAY (IDX-D)     >   ZERO
                       MOVE    IDX-D       TO  SSANTEI-SRYYMD(7:2)
                       MOVE    31          TO  IDX-D
                   END-IF
               END-PERFORM
           ELSE
               MOVE    SPA-SEL-RRKYMD      TO  SSANTEI-SRYYMD
           END-IF
           MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    SRY-PTID            TO  SSANTEI-PTID
           MOVE    SRY-NYUGAIKBN       TO  SSANTEI-NYUGAIKBN
           MOVE    SRY-SRYKA           TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
           IF      (SRY-SRYSURYO(IDZ)  >   1    )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    SRY-SRYSURYO(IDZ)   TO  SSANTEI-SRYSURYO
           END-IF
           MOVE    SPA-SEL-DAY-G       TO  SSANTEI-DAY-AREA
      *    特定薬剤の初回日
           MOVE    SPA-TOKTEI-YMD      TO  SSANTEI-SYOKAIYMD
      *    保険組合せ
           MOVE    SPA-J04-HKNCOMBI    TO  SSANTEI-HKNCOMBINUM
      *    保険区分
           MOVE    SPA-J04-HKNKBN      TO  SSANTEI-HKNKBN
           MOVE    SPA-J04-RSI-HKNKBN  TO  SSANTEI-RSI-HKNKBN
           MOVE    SPA-J04-RSI-JUNKYO  TO  SSANTEI-RSI-JUNKYO
      *
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *    内分泌負荷試験削除フラグ
           IF      SSANTEI-MSANTEIDEL  =   1
               MOVE    1               TO  FLG-NAIBUN
           END-IF
      *
           .
       450122-SANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       45013-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "J04 ZAINUM-SET ERR:" SPA-ERRCD
               GO      TO      45013-ZAINUM-SET-EXT
           END-IF
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
           MOVE    SPA-OPID            TO  PTINF-OPID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 PTINF UPD ERR:"   MCP-RC
                      ",KEY:" PTINF-KEY
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF 
      *
           .
      *
       45013-ZAINUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為内容編集処理
      *****************************************************************
       45101-SINKOUI-HEN-SEC              SECTION.
      *
      *    行為コード
           MOVE    SPA-COM-INPUTCD(IDX)    TO  SRY-SRYCD   (IDS)
      *
      *    約束セット
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
      *        行為コード
               MOVE    SPA-GMN-INPUTCD(IDX)(1:6)   TO  SRY-SRYCD (IDS)
           END-IF
      *
      *    数量
           MOVE    SPA-COM-SURYO  (IDX)    TO  SRY-SRYSURYO(IDS)
      *    回数
           IF     (SPA-COM-DATAKBN (IDX)   =   "3"  )
      *            画像診断のみ
            AND   (SPA-COM-SRYKBN  (IDX)   =   "70" )
               MOVE    SPA-COM-BUNGSU (IDX)
                                           TO  SRY-SRYKAISU(IDS)
           END-IF
      *
      *    外用の個別回数を設定
           IF      SPA-COM-SRYKBN (IDX)    =   "23"
               MOVE    SPA-COM-KAISU2 (IDX) TO  SRY-SRYKAISU(IDS)
           END-IF
      *    自動算定区分
           MOVE    SPA-COM-AUTOKBN (IDX)   TO  SRY-AUTOKBN (IDS)
      *    数量の入力がある時
           IF     (SPA-COM-AUTOKBN (IDX)   =   SPACE )  AND
                  (SPA-COM-SURFLG  (IDX)   =   "1"   )
               MOVE    "S"                     TO  SRY-AUTOKBN (IDS)
           END-IF
      *!!
      *    器材商品コード対応
           IF     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7"   )
             AND  (SPA-COM-AUTOKBN2 (IDX)  =   "1"        )
               MOVE    "1"                 TO  SRY-MEISKYFLG (IDS)
           END-IF
      *!!!!
      *    関連コメント指示
           IF      SPA-COM-INPUTCONT(IDX)  =   "1"
               MOVE    "2"                 TO  SRY-MEISKYFLG (IDS)
           END-IF
      *    内服種類数指示
           IF      SPA-COM-INPUTNAIF(IDX)  =   "1"
               MOVE    "3"                 TO  SRY-MEISKYFLG (IDS)
           END-IF
      *!!
      *    時間外区分入力がある時、英字変換後セット
           IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                PERFORM 45022-JIKANGAI-SET-SEC
           END-IF
      *    入力コメント番号
           IF     (SPA-COM-MEIFLG (IDX)    =   "1"     )  OR
                  (SPA-COM-INPUTCD(IDX)(1:2)   =   "83"
                                               OR  "84" )  OR
                  (SPA-COM-INPUTCD(IDX)(1:4)   =   "0083"
                                               OR  "0084" )
      *     用法コメント
            OR   ((SPA-COM-INPUTCD(IDX)(1:3)   =   "001" )  AND
                  (SPA-COM-YCOMKBN(IDX)        =   1     ))
      *H30.4
            OR    (SPA-COM-INPUTCD(IDX)(1:7)  =   "0992081")
      *R02.4
              OR  (SPA-COM-INPUTCD(IDX)(1:2)  =   "85" )
      *
               IF      (SPA-COM-INPUTCD(IDX)(1:4)  =   "0085"
                                                   OR  "0086" ) AND
                       (SPA-GMN-MEISYO (IDX)   =   SPA-COM-NAME (IDX))
                   CONTINUE
               ELSE
                   PERFORM 450111-KNJCOM-SYROI-SEC
               END-IF
           END-IF
      ***  IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "8" )  OR
      *           (SPA-COM-INPUTCD(IDX)(1:3)   =   "008")
      *        PERFORM 450111-KNJCOM-SYROI-SEC
      *******  MOVE    WRK-MEINUM          TO  SRY-INPUTNUM (IDS)
      **** END-IF
      *    自費
      *****IF      SPA-COM-SRYKBN (IDX)    =   "95" OR "96"
           IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
               MOVE    SPA-COM-JIHIMONEY(IDX)  TO  SRY-JIHIMONEY(IDS)
               ADD     SPA-COM-JIHIMONEY(IDX)  TO  WRK-KINGAK
               MOVE    WRK-KINGAK              TO  SRY-JIHIMONEYTOTAL
           END-IF
      *
      *    画面の計を集計する（１剤で１件）
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
               IF     (SPA-COM-SRYKBN (IDX)    =   "95" OR "96" )
               AND    (SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"  )
                   MOVE    WRK-KINGAK              TO  WRK-ZAITEN
               ELSE
                   MOVE    SPA-COM-TENSU  (IDX)    TO  WRK-ZAITEN
               END-IF
               MOVE    SPA-COM-SYUTEN1(IDX)    TO  WRK-SYUTEN1
               MOVE    SPA-COM-SYUTEN2 (IDX)   TO  WRK-SYUTEN2
               MOVE    SPA-COM-YKZTEN  (IDX)   TO  WRK-YKZTEN
               MOVE    SPA-COM-KIZAITEN(IDX)   TO  WRK-KIZAITEN
      *
               IF      SPA-COM-YAKUHYO (IDX)   =   "1"
                   MOVE    1                   TO  WRK-ZAIKBN
               END-IF
               IF      SPA-COM-GENTEN  (IDX)   =   "1"
                   MOVE    2                   TO  WRK-ZAIKBN
               END-IF
      *TEST
      *H28.9
      *        持参薬
               IF      SPA-COM-JISANYAKU(IDX)  =   "1"
                   MOVE    3                   TO  WRK-ZAIKBN
               END-IF
               IF      SPA-COM-JISANYAKU(IDX)  =   "2"
                   MOVE    4                   TO  WRK-ZAIKBN
               END-IF
      *TEST
           END-IF
      *****IF      SPA-COM-KEIFLG (IDX)    =   "1" OR  "2"
      *        MOVE    SPA-COM-KEI  (IDX)      TO  SRY-JIHIMONEY
      *        MOVE    SPA-COM-KEI  (IDX)      TO  WRK-ZAITEN
      *****END-IF
      *
      *    各附加情報
           PERFORM 450112-WKSRYACTPLUS-SEC
      *
      *    集計
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
               CONTINUE
           ELSE
      *        診療コード計
               MOVE    SPA-COM-INPUTCD(IDX)    TO  WRK-SRYCD-9
               ADD     WRK-SRYCD-9             TO  WRK-SRYCDKEI
      *        数量
               ADD     SPA-COM-SURYO  (IDX)    TO  WRK-SURYOKEI
           END-IF
      *
      *    明細数
           ADD     1                       TO  WRK-MEISAISU
           .
       45101-SINKOUI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分 編集処理
      *****************************************************************
       45022-JIKANGAI-SET-SEC            SECTION.
      *
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)       NUMERIC   ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)       =   SPACE ) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)   NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "A"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "2"
                       MOVE    "B"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "3"
                       MOVE    "C"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "4"
                       MOVE    "D"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "5"
                       MOVE    "E"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "6"
                       MOVE    "F"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "7"
                       MOVE    "G"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "8"
                       MOVE    "H"      TO  SRY-AUTOKBN  (IDS)
               END-EVALUATE
           END-IF
           .
       45022-JIKANGAI-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者コメント作成処理
      *****************************************************************
       450111-KNJCOM-SYROI-SEC           SECTION.
      *
      *
           INITIALIZE                  PTCOM-REC
      *医療機関ＩＤ
           MOVE    SPA-HOSPNUM         TO  PTCOM-HOSPNUM
      *患者ＩＤ
           MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
      *剤番号
      *****MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    WRK-ZAINUM          TO  PTCOM-ZAINUM
      *診療コード
           MOVE    SRY-SRYCD   (IDS)   TO  PTCOM-SRYCD
      *入力コメント
           IF      SPA-COM-INPUTCD(IDX)    =   "810000001"
                                           OR  "008500000"
                                           OR  "008600000"
               MOVE    SPA-GMN-MEISYO-G (IDX)
                                           TO  PTCOM-INPUTCOMENT
           ELSE
               MOVE    SPA-GMN-MEISYO   (IDX)
                                           TO  PTCOM-INPUTCOMENT
           END-IF
      *用法コメント
           IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "001" )
               MOVE    SPA-COM-NAME (IDX)  TO  PTCOM-INPUTCOMENT
           END-IF
      *入力値
           MOVE    SPA-COM-INPUTCHI-G (IDX)
                                       TO  PTCOM-INPUTCHI-AREA
      *コメント枝番番号
           ADD     1                   TO  WRK-MEINUM
           MOVE    WRK-MEINUM          TO  PTCOM-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  PTCOM-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCOM-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCOM-UPHMS
           MOVE    SPA-OPID            TO  PTCOM-OPID
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 PTCOM INS ERR;"    MCP-RC
                       ",KEY:" PTCOM-KEY
               MOVE    "1029"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-MEINUM          TO  SRY-INPUTNUM (IDS)
      *
           .
       450111-KNJCOM-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為新規レコード編集
      *****************************************************************
       45102-SINKOU-INS-SEC            SECTION.
      *
      *H27.10
      *    登録時自動発生解除処理
           ADD     1                   TO  IDX-SR
           PERFORM VARYING     IDXS    FROM    1   BY  1
                   UNTIL      (IDXS        >   5  )
                          OR  (FLG-HENKOU  =   1  )
                          OR  (IDX-SR      >   10 )
               IF     (TBL-SRY-SRYCD (IDX-SR IDXS)
                                       NOT =   SRY-SRYCD (IDXS))
               OR     (TBL-SRY-SRYSURYO (IDX-SR IDXS)
                                       NOT =   SRY-SRYSURYO (IDXS))
               OR     (TBL-SRY-INPUTNUM (IDX-SR IDXS)
                                       NOT =   SRY-INPUTNUM (IDXS))
                   MOVE    1               TO  FLG-HENKOU
               END-IF
           END-PERFORM
      *    変更ありは、登録時自動算定解除
           IF      FLG-HENKOU          =   1
               PERFORM VARYING     IDXS    FROM    1   BY  1
                       UNTIL       IDXS    >   5
                   IF     (SRY-AUTOKBN (IDXS)  =   "3"
                                               OR  "4"
                                               OR  "5"
                                               OR  "6")
                       MOVE    SPACE           TO  SRY-AUTOKBN (IDXS)
                   END-IF
               END-PERFORM
           END-IF
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    SPA-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
      *****MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
           MOVE    WRK-RENNUM          TO  SRY-RENNUM
      *
           MOVE    SPA-COM-SRYSYUKBN(01)
                                       TO  SRY-SRYSYUKBN
           MOVE    SPA-J04-SRYKBN      TO  SRY-SRYKBN
      *
           MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
           MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
           MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
           MOVE    SPA-OPID            TO  SRY-OPID
      *
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACT INS ERR;"    MCP-RC 
                    ",SRY-KEY:"  SRY-KEY
               MOVE    "1024"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO                TO  IDS
           INITIALIZE                  SRYACT-REC
      *
           .
       45102-SINKOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ更新処理
      *****************************************************************
       4502-SINKAI-KOU-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SPA-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT      NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      4502-SINKAI-KOU-EXT
           END-IF
           IF      WRK-ZAITEN          =   ACCT-ZAITEN
      *
           AND    (SPA-J04-JIHOKBN     =   ACCT-JIHOKBN )
               MOVE    ZERO            TO  FLG-SYUNOU-ERR
           ELSE
               MOVE    1               TO  FLG-SYUNOU-ERR
           END-IF
      *
           MOVE    SPA-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *    剤点数
           MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SURYOKEI        TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *    剤識別区分
           MOVE    WRK-ZAIKBN          TO  ACCT-ZAIKBN
      *!!
      *    附加情報あり
           IF      WKSRYP-ADDKBN       =   "1"
      *        剤請求フラグ
               MOVE    "1"                 TO  ACCT-ZAIREQFLG
           ELSE
               IF      FLG-GEN             =   1
                   MOVE    "2"                 TO  ACCT-ZAIREQFLG
               ELSE
                   MOVE    SPACE               TO  ACCT-ZAIREQFLG
               END-IF
           END-IF
      *    包括診療剤（自保区分）
           MOVE    SPA-J04-JIHOKBN     TO  ACCT-JIHOKBN
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
           MOVE    SRYACCT-REC         TO  HZN2-SRYACCT-REC
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACCT UPD ERR:"  MCP-RC
                      ",ACCT-KEY:" ACCT-KEY
               MOVE    "1099"          TO  SPA-ERRCD
           END-IF
      *    受診履歴修正
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (SPA-HKNCOMBINUM NOT =   SPA-J04-HKNCOMBI )
               PERFORM 45021-JYURRK-HKNUPD-SEC
           END-IF
           MOVE    HZN2-SRYACCT-REC    TO  SRYACCT-REC
      *!!
      *    診療会計附加情報処理 
           PERFORM 45023-SRYACCTPLUS-SYORI-SEC
      *!!
      *
      *    算定履歴追加
           PERFORM 450422-SANTEI-UPD-SEC
      *    収納変更処理
           IF     (SPA-NYUGAIKBN       =   "2" )  AND
                  (FLG-SYUNOU-ERR      =   1   )
               PERFORM 450423-SYUNOU-UPD1-SEC
           END-IF
      *
      *    レセプト作成管理マスタ更新処理
           IF     (SPA-NYUGAIKBN       =   "2" )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *         包括診療対象外
              AND (ACCT-JIHOKBN    NOT =   "1"   )
               PERFORM 45021-RECEUPD-UPDATE-SEC
           END-IF
      *
           .
      *
       4502-SINKAI-KOU-EXT.
           EXIT.
      *****************************************************************
      *   受診履歴修正処理
      *****************************************************************
       45021-JYURRK-HKNUPD-SEC     SECTION.
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF      SPA-SEL-DAY (IDX)   NOT =   ZERO
                   MOVE    IDX             TO  WRK-DD
                   PERFORM 450211-JYURRK-UPD-SYORI-SEC 
               END-IF
           END-PERFORM
           .
       45021-JYURRK-HKNUPD-EXT.
           EXIT.
      *****************************************************************
      *   受診履歴修正処理
      *****************************************************************
       450211-JYURRK-UPD-SYORI-SEC     SECTION.
      *
      *    変更前の受診履歴の剤削除
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  JYURRK-SRYYMD(1:6)
           MOVE    WRK-DD              TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-J04-HKNCOMBI    TO  JYURRK-HKNCOMBINUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  IDX-J
           MOVE    ZERO                TO  TBL-ZAINUM-AREA
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1  )
               IF      IDX-J               =   ZERO
                   MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
               END-IF
               IF      (JYURRK-RENNUM      =   HZN-JYURRK-RENNUM)
                   AND (JYURRK-DOUJI-RENNUM
                                           =   HZN-JYURRK-DOUJI-RENNUM)
                   PERFORM VARYING     IDX-J2  FROM    1   BY  1
                           UNTIL       IDX-J2  >   15
                       IF      JYURRK-ZAINUM (IDX-J2)  =  SPA-ZAINUM
                                                       OR  ZERO
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDX-J
                           MOVE    JYURRK-ZAINUM (IDX-J2)
                                                   TO  TBL-ZAINUM
                                                              (IDX-J)
                           PERFORM 4502111-SRYKBN-HEN-SEC
                       END-IF
                   END-PERFORM
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  IDX-J3
           MOVE    HZN-JYURRK-REC      TO  JYURRK-REC
      *
           INITIALIZE                      JYURRK-ZAINUM-G
           INITIALIZE                      JYURRK-SRYKBN-G
           MOVE    ZERO                TO  JYURRK-EDANUM
           PERFORM VARYING     IDX-J2  FROM    1   BY  1
                   UNTIL       IDX-J2  >   IDX-J
               ADD     1                   TO  IDX-J3
               IF      IDX-J3              >   15
                   ADD     1               TO  JYURRK-EDANUM
                   PERFORM 450211-JYURRK-INS-SEC
                   MOVE    1               TO  IDX-J3
                   INITIALIZE                  JYURRK-ZAINUM-G
                   INITIALIZE                  JYURRK-SRYKBN-G
               END-IF
               MOVE    TBL-ZAINUM (IDX-J2) TO  JYURRK-ZAINUM (IDX-J3)
               MOVE    TBL-SRYKBN (IDX-J2) TO  WRK-SRYKBN
               PERFORM 450215-JYURRK-SRYKBN-SEC
           END-PERFORM
           IF      IDX-J3              >   ZERO
               ADD     1               TO  JYURRK-EDANUM
               PERFORM 450211-JYURRK-INS-SEC
           END-IF
      *
      *    変更前の受診履歴の剤追加
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  JYURRK-SRYYMD(1:6)
           MOVE    WRK-DD              TO  JYURRK-SRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  FLG-UPD
           MOVE    ZERO                TO  FLG-UPD2
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-KEY-DOUJI-RENNUM
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1  )
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
               IF      JYURRK-EDANUM       =   1
                   MOVE    ZERO                TO  FLG-UPD2
                   ADD     1                   TO  WRK-RENNUM
                   IF      WRK-RENNUM      >   1
                       ADD     1           TO  WRK-DOUJI-RENNUM
                   END-IF
                   MOVE    JYURRK-DOUJI-RENNUM
                                           TO  WRK-KEY-DOUJI-RENNUM
                   IF     JYURRK-DOUJI-RENNUM  NOT =   WRK-DOUJI-RENNUM
                       MOVE    WRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
                       MOVE    1               TO  FLG-UPD2
                   END-IF
               ELSE
                   IF     JYURRK-DOUJI-RENNUM  =   WRK-KEY-DOUJI-RENNUM
                       MOVE    WRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
                   END-IF
               END-IF
               IF      SPA-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
                    PERFORM VARYING     IDX-J2  FROM    1   BY  1
                            UNTIL       IDX-J2  >   15
                       IF      JYURRK-ZAINUM (IDX-J2)  =   ZERO
                           MOVE    WRK-ZAINUM  TO  JYURRK-ZAINUM(IDX-J2)
                           MOVE    1           TO  FLG-UPD
                           MOVE    1           TO  FLG-UPD2
                           MOVE    15          TO  IDX-J2
                           MOVE    ZERO        TO  FLG-ARI
                       END-IF
                   END-PERFORM
                   IF      FLG-UPD2            =   ZERO
                       MOVE    JYURRK-REC      TO  HZN2-JYURRK-REC
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-IF
      *
               IF     (FLG-UPD2        =   1  )
                   PERFORM 450211-JYURRK-UPD-SEC
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-UPD             =   ZERO
               IF      FLG-ARI             =   1
      *            枝番＋１で追加
                   MOVE    HZN2-JYURRK-REC     TO  JYURRK-REC
                   ADD     1                   TO  JYURRK-EDANUM
                   INITIALIZE                      JYURRK-ZAINUM-G
                   MOVE    WRK-ZAINUM          TO  JYURRK-ZAINUM(1)
                   PERFORM 450211-JYURRK-INS-SEC
               ELSE
      *            新規追加
                   PERFORM 450215-JYURRK-SAKUSEI-SEC
               END-IF
           END-IF
           .
      *
       45021-JYURRK-HKNUPD-EXT.
           EXIT.
      *****************************************************************
      *   受診履歴更新処理
      *****************************************************************
       4502111-SRYKBN-HEN-SEC     SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM (IDX-J2)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACCT         =   ZERO
               IF      ACCT-DAY (1 WRK-DD) >   ZERO
                   MOVE    ACCT-SRYKBN     TO  TBL-SRYKBN (IDX-J)
               END-IF
           END-IF
      *
           .
      *
       4502111-SRYKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *   受診履歴更新処理
      *****************************************************************
       450211-JYURRK-INS-SEC     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
              DISPLAY "J04 JYURRK INS ERR:" JYURRK-KEY  "."
              DISPLAY "MCP-RC:" MCP-RC ":" 
              MOVE    "1099"          TO  SPA-ERRCD
           END-IF
           .
       450211-JYURRK-INS-EXT.
           EXIT.
      *****************************************************************
      *   受診履歴更新処理
      *****************************************************************
       450211-JYURRK-UPD-SEC     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
              DISPLAY "J04 JYURRK UPD ERR:" JYURRK-KEY  "."
              DISPLAY "MCP-RC:" MCP-RC ":" 
              MOVE    "1099"          TO  SPA-ERRCD
           END-IF
           .
       450211-JYURRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴作処理
      *****************************************************************
       450215-JYURRK-SAKUSEI-SEC     SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  JYURRK-SRYYMD(1:6)
           MOVE    WRK-DD              TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-HKNKBN          TO  JYURRK-HKNKBN
           IF      SPA-HKNKBN          =   1
               EVALUATE    SPA-RSI-HKNKBN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
      *                 労災
                       MOVE    "1"                 TO  JYURRK-HKNKBN
                   WHEN    "4"
      *                自賠責
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "4"             TO  JYURRK-HKNKBN
                       ELSE
      *                    労災準拠
                           MOVE    "2"             TO  JYURRK-HKNKBN
                       END-IF
                   WHEN    "5"
      *                労務災害
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "5"             TO  JYURRK-HKNKBN
                       ELSE
      *                    労災準拠
                           MOVE    "1"             TO  JYURRK-HKNKBN
                       END-IF
      *H25.7
                   WHEN    "6"
      *                第三者行為
                       MOVE    "7"                 TO  JYURRK-HKNKBN
               END-EVALUATE
           END-IF
      *    公害
           IF      SPA-HKNKBN          =   2
               MOVE    "6"                 TO  JYURRK-HKNKBN
           END-IF
           MOVE    ZERO                TO  JYURRK-DENPNUM
      *
           MOVE    1                   TO  JYURRK-RENNUM
           MOVE    1                   TO  JYURRK-EDANUM
           IF      WRK-RENNUM      NOT =   ZERO
               ADD     1                   TO  WRK-DOUJI-RENNUM
           END-IF
           MOVE    WRK-DOUJI-RENNUM    TO  JYURRK-DOUJI-RENNUM
      *    会計連番
           IF      JYURRK-KAIKEI-RENNUM    =   ZERO
               MOVE    1               TO  JYURRK-KAIKEI-RENNUM
           END-IF
      *
           MOVE    WRK-ZAINUM          TO  JYURRK-ZAINUM (1)
           MOVE    SPA-J04-SRYKBN      TO  WRK-SRYKBN
           PERFORM 450215-JYURRK-SRYKBN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
           PERFORM 450211-JYURRK-INS-SEC
           .
       450215-JYURRK-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴種別編集処理
      *****************************************************************
       450215-JYURRK-SRYKBN-SEC           SECTION.
      *
           EVALUATE   WRK-SRYKBN
               WHEN   "11"
               WHEN   "12"
               WHEN   "13"
               WHEN   "14"
                   MOVE    "01"        TO  JYURRK-SRYKBN1
               WHEN   "21"
                   MOVE    "01"        TO  JYURRK-SRYKBN2
               WHEN   "22"
                   MOVE    "01"        TO  JYURRK-SRYKBN3
               WHEN   "23"
                   MOVE    "01"        TO  JYURRK-SRYKBN4
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "01"        TO  JYURRK-SRYKBN5
               WHEN   "40"
                   MOVE    "01"        TO  JYURRK-SRYKBN6
               WHEN   "50"
                   MOVE    "01"        TO  JYURRK-SRYKBN7
               WHEN   "54"
                   MOVE    "01"        TO  JYURRK-SRYKBN8
               WHEN   "60"
               WHEN   "64"
                   MOVE    "01"        TO  JYURRK-SRYKBN9
               WHEN   "70"
                   MOVE    "01"        TO  JYURRK-SRYKBN10
               WHEN   "80"
                   MOVE    "01"        TO  JYURRK-SRYKBN11
           END-EVALUATE
           .
       450215-JYURRK-SRYKBN-EXT.
           EXIT.
      *****************************************************************
      *   診療会計附加情報処理
      *****************************************************************
       45023-SRYACCTPLUS-SYORI-SEC     SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *    内服逓減は剤変更しない
           MOVE    1                   TO  ACCTP-RENNUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    診療会計附加情報あり
           IF      WKSRYP-ADDKBN       =   "1"
               PERFORM 450231-SRYACCTPLUS-ADD-SEC
           ELSE
      *        削除
               IF      FLG-SRYACCTPLUS     =   ZERO
                   PERFORM 450232-SRYACCTPLUS-DEL-SEC
               END-IF
           END-IF
           .
      *
       45023-SRYACCTPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   附加情報作成処理
      *****************************************************************
       450231-SRYACCTPLUS-ADD-SEC          SECTION.
      *
           IF      FLG-SRYACCTPLUS     =   1
               MOVE    SPACE               TO  SRYACCTPLUS-REC
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    ACCT-HOSPNUM         TO  ACCTP-HOSPNUM
               MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
               MOVE    ACCT-PTID           TO  ACCTP-PTID
               MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
               MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
               MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
               MOVE    1                   TO  ACCTP-RENNUM
           END-IF
      *    点数区分
           MOVE    WKSRYP-PLUSKBN      TO  ACCTP-PLUSKBN
      *    労災の円
           MOVE    WKSRYP-RSIKINGAKU   TO  ACCTP-RSIKINGAKU
      *
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   5
               MOVE    WKSRYP-SRYCD (IDW)
                                           TO  ACCTP-SRYCD (IDW)
               MOVE    WKSRYP-SYUTEN (IDW)
                                           TO  ACCTP-SYUTEN (IDW)
               MOVE    WKSRYP-YKZTEN (IDW)
                                           TO  ACCTP-YKZTEN (IDW)
               MOVE    WKSRYP-KIZAITEN (IDW)
                                           TO  ACCTP-KIZAITEN (IDW)
               MOVE    WKSRYP-FIRTEN (IDW)
                                           TO  ACCTP-FIRTEN (IDW)
           END-PERFORM
      *    酸素点数
           MOVE    WKSRYP-SANSOTEN     TO  ACCTP-SANSOTEN
      *    窒素点数
           MOVE    WKSRYP-CHISOTEN     TO  ACCTP-CHISOTEN
      *    院外処方点数
           MOVE    WKSRYP-INGAITEN     TO  ACCTP-INGAITEN
      *    内服７種類逓減対象剤番号はないものとする
           INITIALIZE                  ACCTP-GRP-ZAINUM-G
      *
           MOVE    SMCNDATE-YMD        TO  ACCTP-CREYMD
           MOVE    SMCNDATE-YMD        TO  ACCTP-UPYMD
      *
           IF      FLG-SRYACCTPLUS     =   1
      *        新規
               MOVE    SMCNDATE-HMS        TO  ACCTP-UPHMS
               MOVE    SPA-OPID            TO  ACCTP-OPID
      *
               MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                  DISPLAY "J04 SRYACCTPLUS INS ERR:" ACCTP-KEY  "."
                  DISPLAY "MCP-RC:" MCP-RC ":" 
                  MOVE    "1032"          TO  SPA-ERRCD
               END-IF
           ELSE
      *        更新
               MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                  DISPLAY "J04 SRYACCTPLUS UPD ERR:" ACCTP-KEY  "."
                  DISPLAY "MCP-RC:" MCP-RC ":" 
                  MOVE    "1032"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       450231-SRYACCTPLUS-ADD-EXT.
           EXIT.
      *****************************************************************
      *   附加情報削除処理
      *****************************************************************
       450232-SRYACCTPLUS-DEL-SEC          SECTION.
      *
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM         TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04  SRYACCTPLUS  DEL ERR:" MCP-RC
                     ",KEY:" ACCTP-KEY
               MOVE    "1032"          TO  SPA-ERRCD
           END-IF
           .
      *
       450232-SRYACCTPLUS-DEL-EXT.
           EXIT.
      *****************************************************************
      *   算定履歴追加処理
      *****************************************************************
       450422-SANTEI-UPD-SEC          SECTION.
      *
           IF     ACCT-HKNCOMBI        =   "9999"
               GO      TO      450422-SANTEI-UPD-EXT
           END-IF
      *H27.10 削除
      *****包括診療対象外
      *    IF      ACCT-JIHOKBN        =   "1"
      *        GO      TO      450422-SANTEI-UPD-EXT
      **** END-IF
      *
      *    自費の時対象外
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               GO      TO      450422-SANTEI-UPD-EXT
           END-IF
      *
      *    診療会計マスターから診療行為マスターを検索する
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
               PERFORM VARYING     IDZ    FROM    1   BY  1
                       UNTIL      (IDZ        >   5   )  OR
                                  (SRY-SRYCD(IDZ)     =   SPACE )
      *        手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1"   )
      *                システム予約で履歴があるもの
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 450123-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
      *
       450422-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴追加更新処理
      *****************************************************************
       450123-SANTEI-UPD-SEC          SECTION.
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "J04"               TO  SSANTEI-PG
           MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    SRY-PTID            TO  SSANTEI-PTID
           MOVE    SRY-NYUGAIKBN       TO  SSANTEI-NYUGAIKBN
           MOVE    SRY-SRYKA           TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
           IF      (SRY-SRYSURYO(IDZ)  >   1    )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    SRY-SRYSURYO(IDZ)   TO  SSANTEI-SRYSURYO
           END-IF
           MOVE    SPA-SEL-DAY-G       TO  SSANTEI-DAY-AREA
           IF      SPA-SEL-RRKYMD      =   SPACE
               MOVE    SRY-SRYYM           TO  SSANTEI-SRYYMD(1:6)
               MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
               PERFORM   VARYING   IDX-D   FROM    1   BY  1
                         UNTIL     IDX-D       >   31
                   IF      SPA-SEL-DAY (IDX-D)     >   ZERO
                       MOVE    IDX-D       TO  SSANTEI-SRYYMD(7:2)
                       MOVE    31          TO  IDX-D
                   END-IF
               END-PERFORM
           ELSE
               MOVE    SPA-SEL-RRKYMD      TO  SSANTEI-SRYYMD
           END-IF
      *    特定薬剤の初回日
           MOVE    SPA-TOKTEI-YMD      TO  SSANTEI-SYOKAIYMD
      *    内分泌負荷試験用
           IF      WKSRYP-MSANTEITFLG  =   1
               MOVE    WKSRYP-MSANTEITEN
                                        TO  SSANTEI-MSANTEITEN
               MOVE    WKSRYP-MSANTEITENKBN
                                        TO  SSANTEI-MSANTEITENKBN
               MOVE    1                TO  FLG-NAIBUN
           END-IF
      *    保険組合せ
           MOVE    SPA-HKNCOMBINUM     TO  SSANTEI-HKNCOMBINUM
      *    保険区分
           MOVE    SPA-HKNKBN          TO  SSANTEI-HKNKBN
           MOVE    SPA-RSI-HKNKBN      TO  SSANTEI-RSI-HKNKBN
           MOVE    SPA-RSI-JUNKYO      TO  SSANTEI-RSI-JUNKYO
      *
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       450123-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   更新時の収納更新処理
      *****************************************************************
       450423-SYUNOU-UPD1-SEC          SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   15
               MOVE    SPA-ZAINUM          TO  JYURRK-ZAINUM (IDZ)
           END-PERFORM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-UPD
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      JYURRK-SRYYMD(1:6)  =   SPA-J02-SRYYMD(1:6)
      *            収納の会計変更区分変更
                   IF     (SPA-NYUGAIKBN       =   "2"  )
                   AND    (JYURRK-HKNCOMBINUM  NOT =   "9999")
                       PERFORM 450431-SYUNOU-UPD-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       450423-SYUNOU-UPD1-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
       45021-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "2"                 TO  LNK-UPD-SYORI
           MOVE    SPA-HOSPNUM          TO  LNK-UPD-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           MOVE    SPA-NYUGAIKBN       TO  LNK-UPD-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-UPD-SRYYM
           MOVE    SPA-HKNKBN          TO  LNK-UPD-HKNKBN
           IF      SPA-HKNKBN          =   "1"
               EVALUATE    SPA-RSI-HKNKBN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
      *                 労災
                       MOVE    "1"                 TO  LNK-UPD-HKNKBN
                   WHEN    "4"
      *                自賠責
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "4"             TO  LNK-UPD-HKNKBN
                       ELSE
      *                    労災準拠
                           MOVE    "2"             TO  LNK-UPD-HKNKBN
                       END-IF
                   WHEN    "5"
      *                労務災害
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "5"             TO  LNK-UPD-HKNKBN
                       ELSE
      *                    労災準拠
                           MOVE    "1"             TO  LNK-UPD-HKNKBN
                       END-IF
      *H25.7
                   WHEN    "6"
      *                第三者行為
                       MOVE    "7"                 TO  LNK-UPD-HKNKBN
               END-EVALUATE
           END-IF
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
                                                SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
       45021-RECEUPD-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ更新処理
      *****************************************************************
       4505-SRYACCT-DEL-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SPA-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT          =  ZERO
               MOVE    SRYACCT-REC         TO  HZN-SRYACCT-REC
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J04 SRYACCT DEL ERR:"  MCP-RC
                          ",ACCT-KEY:" ACCT-KEY
                   MOVE    "1099"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4505-SRYACCT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスタ追加、受診履歴変更処理
      *****************************************************************
       4504-SRYACCT-ADD-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYKBN-HEN
      *    変更前の診療会計マスタを修正する
           IF      SPA-J04-SYORI       =   ZERO
               IF     (SPA-SEL-RRKYMD      =   SPACE )
                 AND  (SPA-J04-SYORI       =   ZERO  )
                 AND  (SPA-SRYKBN      NOT =   SPA-J04-SRYKBN)
      *            診療区分が変更になった時、診療会計の削除
                   PERFORM 4505-SRYACCT-DEL-SEC
                   MOVE    1               TO  FLG-SRYKBN-HEN
               ELSE
                   PERFORM 45041-SRYACCT-MAE-SEC
               END-IF
           ELSE
               MOVE    ZERO            TO  WRK-DAY
           END-IF
      *
      *    診療会計マスタ追加
           PERFORM 45042-SRYACCT-INS-SEC
      *!!
      *    診療会計附加情報処理 
           PERFORM 45023-SRYACCTPLUS-SYORI-SEC
      *!!
      *
           IF      SPA-J04-SYORI       =   ZERO
      *        受診履歴修正
               IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                      (SPA-HKNCOMBINUM NOT =   SPA-J04-HKNCOMBI)
                   PERFORM 45021-JYURRK-HKNUPD-SEC
               ELSE
      *        受診履歴修正
                   PERFORM 45043-JYURRK-UPD-SEC
               END-IF
           END-IF
           .
      *
       4504-SRYACCT-ADD-EXT.
           EXIT.
      *****************************************************************
      *    前診療会計マスタ修正処理
      *****************************************************************
       45041-SRYACCT-MAE-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SPA-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT      NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      45041-SRYACCT-MAE-EXT
           END-IF
      *
      *    剤回数
           MOVE    SPA-SEL-RRKYMD(7:2) TO  IDX-D2
           COMPUTE IDX-D1              =   SPA-SEL-RENNUM   +  1
           MOVE    ACCT-DAY (IDX-D1 IDX-D2)
                                       TO  WRK-DAY
           COMPUTE ACCT-DAY (1 IDX-D2) =   ACCT-DAY (1 IDX-D2)
                                       -   WRK-DAY
           MOVE    ZERO                TO  ACCT-DAY
                                                (IDX-D1 IDX-D2)
           COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                       -   WRK-DAY
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACCT UPD ERR:"  MCP-RC
                    ",KEY:" ACCT-KEY
               MOVE    "1099"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       45041-SRYACCT-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ追加処理
      *****************************************************************
       45042-SRYACCT-INS-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-J04-SRYKBN      TO  ACCT-SRYKBN
      *
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
      *    保険組合せ
           MOVE    SPA-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *    包括診療剤（自保区分）
           MOVE    SPA-J04-JIHOKBN     TO  ACCT-JIHOKBN
      *    剤点数
           MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SURYOKEI        TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *    剤識別区分
           MOVE    WRK-ZAIKBN          TO  ACCT-ZAIKBN
      *!!
      *    附加情報あり
           IF      WKSRYP-ADDKBN       =   "1"
      *        剤請求フラグ
               MOVE    "1"                 TO  ACCT-ZAIREQFLG
           END-IF
      *
           IF      SPA-J04-SYORI       =   ZERO
               IF      FLG-SRYKBN-HEN      =   1
                   MOVE    HZN-ACCT-ZAIKAISU   TO  ACCT-ZAIKAISU
                   MOVE    HZN-ACCT-DAY-AREA   TO  ACCT-DAY-AREA
               ELSE
      *    剤回数
               MOVE    WRK-DAY             TO  ACCT-ZAIKAISU
      *    診療日
               MOVE    SPA-SEL-RRKYMD(7:2) TO  IDX-D2
               COMPUTE IDX-D1              =   SPA-SEL-RENNUM   +  1
               MOVE    WRK-DAY             TO  ACCT-DAY
                                                 (IDX-D1 IDX-D2)
               MOVE    WRK-DAY             TO  ACCT-DAY
                                                 (1    IDX-D2)
               END-IF
      *
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1026"          TO  SPA-ERRCD
               DISPLAY "J04 SRYACCT INS ERR:"  MCP-RC
                       ",KEY:" ACCT-KEY
           END-IF
      *
      *    算定履歴追加
           IF      SPA-J04-SYORI       =   ZERO
               PERFORM 450422-SANTEI-UPD-SEC
           END-IF
      *
           .
       45042-SRYACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴修正処理
      *****************************************************************
       45043-JYURRK-UPD-SEC          SECTION.
      *
      *    受診履歴の剤変更
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
      **** IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、伝票番号
      *        MOVE    SPA-SEL-DENPNUM     TO  JYURRK-DENPNUM
      *        MOVE    "key6"              TO  WRK-PATH
      **** ELSE
      *H24.5   伝票番号重複対応（伝票番号検索廃止）
      *        入院の時、診療年月日、保険組合せ
               MOVE    SPA-SEL-RRKYMD      TO  JYURRK-SRYYMD
               MOVE    SPA-SEL-RENNUM      TO  JYURRK-RENNUM
               MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
               MOVE    "key19"             TO  WRK-PATH
      **** END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-UPD
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1  )  OR
                              (FLG-UPD         =   1  )
      *        外来の時、伝票番号の一致のチェック
           IF      (JYURRK-NYUGAIKBN     =  "2"  )  AND
                   (SPA-SEL-DENPNUM  NOT =  ZERO )  AND
                   (SPA-SEL-DENPNUM  NOT =  JYURRK-DENPNUM )
               CONTINUE
           ELSE
      *
               IF     (SPA-SEL-RENNUM          =   JYURRK-RENNUM )  AND
                      (SPA-SEL-DOUJI-RENNUM    =   JYURRK-DOUJI-RENNUM)
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   15
                       IF      JYURRK-ZAINUM (IDX)     =   SPA-ZAINUM
                           MOVE    WRK-ZAINUM      TO  JYURRK-ZAINUM
                                                                 (IDX)
                           MOVE    15              TO  IDX
                           MOVE    1               TO  FLG-UPD
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      FLG-UPD             =   1
      *            収納の会計変更区分変更
                   IF     (SPA-NYUGAIKBN       =   "2"   )
                   AND    (JYURRK-HKNCOMBINUM  NOT =   "9999")
                       PERFORM 450431-SYUNOU-UPD-SEC
                   END-IF
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
                   MOVE    SPA-OPID            TO  JYURRK-OPID
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
      *
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       45043-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       450431-SYUNOU-UPD-SEC          SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    JYURRK-HOSPNUM       TO  SYU-HOSPNUM
           MOVE    JYURRK-PTID         TO  SYU-PTID
           MOVE    JYURRK-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
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
               MOVE    "1"                 TO  SYU-ACCT-UPDKBN
      *
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
               MOVE    SPA-OPID            TO  SYU-OPID
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       450431-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計パラメタ更新処理
      *****************************************************************
       4503-PARA-KOU-SEC          SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  SRYACCT-REC
               IF     (ACCT-SRYKBN         =   SPA-SRYKBN)  AND
                      (ACCT-ZAINUM         =   SPA-ZAINUM)
      *            保険組合せ
                   MOVE    SPA-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *            剤点数
                   MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *            診療コード計
                   MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *            数量計
                   MOVE    WRK-SURYOKEI        TO  ACCT-SURYOUTOTAL
      *            明細数
                   MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *            手技点数１
                   MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *            手技点数２
                   MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *            薬剤点数
                   MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *            器材点数
                   MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *            剤識別区分
                   MOVE    WRK-ZAIKBN          TO  ACCT-ZAIKBN
      *
                   MOVE    SRYACCT-REC         TO  PARA-DATA-REC
      *
      *
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J04 PARA UPDATE  ERR:"  MCP-RC
                   END-IF
                   MOVE    1                   TO  FLG-PARA
               ELSE
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       4503-PARA-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *    禁忌チェック
           MOVE    1                   TO  IDX-K1
           INITIALIZE                  WRK-KNKCHK-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"   )  AND
                      (SPA-COM-KNKUMU (IDX)        =   SPACE )  AND
                      (SPA-COM-SRYKBN (IDX)        =   "21" OR  "22"
                                                   OR "23"  OR  "31"
                                                   OR  "32" OR "33")
                   MOVE    ZERO                TO  IDX-K
      *           チェックレコードより、禁忌有無のチェックを行う
                   INITIALIZE                  CHK-REC
                   MOVE    "4"                 TO  CHK-CHKKBN
                   MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  CHK-SRYCD
                   MOVE    "2"                 TO  CHK-CDKBN
                   MOVE    SPA-J02-SRYYMD      TO  CHK-YUKOSTYMD
                   MOVE    SPA-J02-SRYYMD      TO  CHK-YUKOEDYMD
      *
                   MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
                   MOVE    CHK-REC             TO  MCPDATA-REC
                   MOVE    "tbl_chk"           TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_chk"           TO  MCP-TABLE
                       MOVE    "key3"              TO  MCP-PATHNAME
                       PERFORM 950-CHK-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-CHKMST
                   END-IF
                   PERFORM
                           UNTIL      FLG-CHKMST   =   1
      **********       PERFORM VARYING     IDX-C   FROM    1   BY  1
      *                        UNTIL      (IDX-C           >   10 ) OR
      ******                              (CHK-CD (IDX-C)  =   SPACE)
                           PERFORM 410011-YAKKNK-SYORI-SEC
      *******          END-PERFORM
      *
                       MOVE    "tbl_chk"           TO  MCP-TABLE
                       MOVE    "key3"              TO  MCP-PATHNAME
                       PERFORM 950-CHK-READ-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_chk"           TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
                   IF      IDX-K             >   ZERO
                      MOVE    SPA-COM-INPUTCD (IDX) TO WRK-KNK-TOUSRYCD
                                                              (IDX-K1)
                      MOVE    SPA-GMN-MEISYO  (IDX) TO  WRK-KNK-TOUMEI
                                                              (IDX-K1)
                      ADD     1                     TO  IDX-K1
                   END-IF
               END-IF
           END-PERFORM
      *
      *    テーブル再編集処理
           PERFORM 410012-YAKKNK-TBL-SEC
      *
           .
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤チェック処理
      *****************************************************************
       410011-YAKKNK-SYORI-SEC           SECTION.
      *
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   100 )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
               IF      SPA-KNK-SRYCD (IDX-S)   =   CHK-CD
                   PERFORM VARYING     IDX-S2      FROM    1   BY  1
                           UNTIL       IDX-S2      >   12
                       IF      SPA-KNK-SRYYM (IDX-S IDX-S2)
                                                   NOT =   SPACE
                           ADD     1               TO  IDX-K
                           MOVE    SPA-KNK-SRYMEI (IDX-S) 
                                                   TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K)
                           MOVE    SPA-KNK-SRYCD  (IDX-S)
                                                   TO  WRK-KNK-SRYCD
                                                         (IDX-K1 IDX-K)
                           MOVE    SPA-KNK-SRYYM (IDX-S IDX-S2)
                                                   TO  WRK-KNK-SRYYM
                                                         (IDX-K1 IDX-K)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   40  )  OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE )
               IF     (SPA-COM-SRYKBN  (IDX-S)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32"  OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-S)     =   CHK-CD )
                 AND  (IDX-S                   NOT =   IDX            )
                   ADD     1                       TO  IDX-K
                   MOVE    SPA-GMN-MEISYO (IDX-S)  TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K)
                   MOVE    SPA-GMN-INPUTCD(IDX-S)  TO  WRK-KNK-SRYCD
                                                        (IDX-K1 IDX-K)
                   MOVE    SPA-J02-SRYYMD(1:6)     TO  WRK-KNK-SRYYM
                                                        (IDX-K1 IDX-K)
               END-IF
           END-PERFORM
           .
       410011-YAKKNK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤チェックテーブル再編集
      *****************************************************************
       410012-YAKKNK-TBL-SEC           SECTION.
      *
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (WRK-KNK-TOUSRYCD (IDX-K1)  =   SPACE )
      *        薬剤ごとに投与年月を降順にソートする
               MOVE    WRK-KNK-TBL2A(IDX-K1)   TO  WRK-SRT-TBL2A
               INITIALIZE                      WRK-KNK-TBL2A(IDX-K1)
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   MOVE    LOW-VALUE           TO  WRK-KEY-SRYYM
                   MOVE    HIGH-VALUE          TO  WRK-KEY-SRYCD
                   MOVE    ZERO                TO  IDX2
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL       IDX1        >   30
                       EVALUATE    TRUE
                         WHEN    WRK-SRT-SRYYM (IDX1) >  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYYM (IDX1)
                                                   TO  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYCD   (IDX1)
                                                   TO  WRK-KEY-SRYCD
                               MOVE    IDX1        TO  IDX2
                         WHEN   (WRK-SRT-SRYYM (IDX1) =  WRK-KEY-SRYYM)
                            AND (WRK-SRT-SRYCD (IDX1) <  WRK-KEY-SRYCD)
                               MOVE    WRK-SRT-SRYYM (IDX1)
                                                   TO  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYCD   (IDX1)
                                                   TO  WRK-KEY-SRYCD
                               MOVE    IDX1        TO  IDX2
                         WHEN   (WRK-SRT-SRYYM (IDX1) = WRK-KEY-SRYYM)
                            AND (WRK-SRT-SRYCD (IDX1) = WRK-KEY-SRYCD)
                               MOVE    WRK-SRT-SRYYM (IDX1)
                                                   TO  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYCD   (IDX1)
                                                   TO  WRK-KEY-SRYCD
                               MOVE    IDX1        TO  IDX2
                       END-EVALUATE
                   END-PERFORM
                   IF      IDX2                >   ZERO
                       MOVE    WRK-SRT-TBL2(IDX2)  TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K)
                       INITIALIZE                  WRK-SRT-TBL2 (IDX2)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       410012-YAKKNK-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示（入力ごと）
      *****************************************************************
       510229-POP-ERR-SEC              SECTION.
      *
           MOVE    SPACE               TO  J042
           INITIALIZE                      J042
           MOVE    WRK-KNK-TOUMEI (1)  TO  J042-POP-TOUMEI
      *
           MOVE    ZERO                TO  IDX-X
           MOVE    SPACE               TO  WRK-KNKMEI-G
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   30  )   OR
                              (IDX-X       >=  3   )  
               IF      WRK-KNK-KNKMEI (1 IDX-K)    NOT =   SPACE
                   ADD     1               TO  IDX-X
                   MOVE    WRK-KNK-KNKMEI (1 IDX-K)
      *********                            TO  J042-KNKMEI (IDX-X)
                                           TO  WRK-KNKMEI (IDX-X)
                   MOVE    X"0A"           TO  WRK-KNKCUR (IDX-X)
               END-IF
           END-PERFORM
           MOVE    WRK-KNKMEI-G        TO  J042-LIST
      *
      *    MOVE    IDX-X               TO  J042-COUNT
      *    初期カーソル設定
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J042"              TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J042"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510229-POP-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示（登録前）
      *****************************************************************
       410019-POP-J041-SEC              SECTION.
      *
           MOVE    SPACE               TO  J041
      *
           INITIALIZE                      J041
           MOVE    1                   TO  IDX-K
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   30  )   OR
                              (IDX-K       >=  50  )
                IF      WRK-KNK-TOUMEI (IDX1)  NOT =   SPACE
                    MOVE    WRK-KNK-TOUMEI (IDX1)  TO  J041-TOUMEI
                                                            (IDX-K)
                END-IF
                PERFORM VARYING    IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   30  )   OR
                                  (WRK-KNK-TOUMEI (IDX1)   =   SPACE )
                   IF      WRK-KNK-KNKMEI (IDX1 IDX2)  NOT =   SPACE
                       MOVE    WRK-KNK-KNKMEI (IDX1 IDX2)
                                               TO  J041-KNKMEI (IDX-K)
                       MOVE    WRK-KNK-SRYYM(IDX1 IDX2)
                                               TO  WRK-SYMD(1:6)
                       MOVE    01              TO  WRK-SDD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  J041-TOUYM  (IDX-K)
                       ADD     1               TO  IDX-K
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      IDX-K               >   1
               COMPUTE IDX-K           =   IDX-K       -   1
           END-IF
           MOVE    IDX-K               TO  J041-COUNT
      *
      *    COMPUTE SPA-J041-PAGE       =  (IDX-K       -    1  )
      *                                /   20
      *                                +   1
      *
      *    初期カーソル設定
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J041"              TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J041"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410019-POP-J041-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
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
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    展開時のエラー
           IF     (SPA-TENKAI-ERR      =   1
                                       OR  2  )  AND
                  (SPA-TENKAI-CHK      =   ZERO)
               IF      SPA-ERRCD           =   "K103"
                                           OR  "K104"
                   CONTINUE
               ELSE
                   MOVE    "K103"              TO  SPA-ERRCD
                   IF      SPA-TENKAI-ERR      =   2
                       MOVE    "K104"              TO  SPA-ERRCD
                   END-IF
                   MOVE    1                   TO  SPA-TENKAI-CHK
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *TEST
      *    コメント入力位置エラーはカーソル移動のみ
           IF      SPA-ERRCD           =   "0100"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *TEST
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-JIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-J04SPA-OUT-SEC
      *
           MOVE    SPACE               TO  J04
           INITIALIZE                      J04
      *
           MOVE    ZERO                TO  IDX
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   SPA-MAX-LINE  )  OR
                              (IDX         >=  MAX-GMN   )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  J04-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  J04-INPUTCD(IDX)
      *
      *H30.9       選択式コメントあり
                   IF      SPA-COM-RECEKISAI (WRK-IDX) NOT =  SPACE
                       MOVE   "S"       TO  SPA-GMN-MEIH (WRK-IDX)(2:1)
                   END-IF
      *
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  J04-MEISYO(IDX)
                   MOVE    "black"             TO  J04-MEISYO-STYLE(IDX)
      *!!          禁忌薬剤
                   IF      SPA-COM-PTKINKI-ARI (WRK-IDX)   =   1
                       MOVE    "red"          TO  J04-MEISYO-STYLE(IDX)
                   END-IF
      *!
                   PERFORM 5002-SURYO-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PTNUM       TO  J04-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  J04-KANANAME
           MOVE    SPA-GMN-NAME        TO  J04-NAME
           MOVE    SPA-GMN-SEX         TO  J04-SEX
      *    MOVE    SPA-J02-HKNCOMBIMEI TO  J04-HKNCOMBI
           MOVE    SPA-J02-FTNRATEMEI  TO  J04-RATE
           MOVE    SPA-J02-SRYYMDW(1:6)    TO  J04-SRYYM
           MOVE    SPA-GMN-BIRTHDAYW   TO  J04-BIRTHDAY
           MOVE    SPA-J01-SRYKAMEI    TO  J04-SRYKA
      *
           MOVE    SPA-GMN-HKNCOMBI-G  TO  J04-HKNCOMBI
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-HKN-MAX
               MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                       TO  J04-HKNCOMBI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  J04-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-HKTKBN-G  TO  J04-HKTKBN
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-HKTKBN-MAX
               MOVE    SPA-GMN-HKTKBN-LST (IDX)
                                       TO  J04-HKTKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-HKTKBN-MAX      TO  J04-HKTKBN-COUNT
      *
           MOVE    SPA-SEL-RRKYMDG     TO  J04-RRKYMD
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               MOVE    SPA-SEL-DAY (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ               TO  J04-DAY(IDX)
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
               PERFORM 5001-MAPCUR-SEC
           END-IF
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       5002-GMNSPA-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-MAP-MAX
           MOVE    ZERO                TO  WRK-MAP-CUR
           MOVE    ZERO                TO  WRK-MAP-CUR2
           MOVE    ZERO                TO  WRK-MAP-CUR3
           MOVE    ZERO                TO  WRK-MAP-MAXPAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE2
           MOVE    ZERO                TO  WRK-MAP-PAGE3
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   MAX-GMN
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"
                           IF      WRK-MAP-CUR         =   ZERO
                               MOVE    IDX            TO  WRK-MAP-CUR
                              MOVE    WRK-PAGE        TO  WRK-MAP-PAGE
                           END-IF
                       END-IF
      *                警告位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "2"
                           IF      WRK-MAP-CUR2        =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR2
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE2
                           END-IF
                       END-IF
      *                フリーコメントで未入力の行
                       IF     (SPA-COM-IN2 (WRK-IDX)   =   "3" ) AND
                              (SPA-COM-IN  (WRK-IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
      *    画面カーソル位置決定（警告はエラーの次に）
           MOVE    ZERO                TO  SPA-MAP-IDX
           IF      WRK-MAP-CUR         =   ZERO
               IF      WRK-MAP-CUR2    NOT =   ZERO
                   MOVE    WRK-MAP-CUR2        TO  SPA-MAP-IDX
                   MOVE    WRK-MAP-PAGE2       TO  SPA-GMN-PAGE
               END-IF
           ELSE
               MOVE    WRK-MAP-CUR         TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE        TO  SPA-GMN-PAGE
           END-IF
      *
           IF      SPA-MAP-IDX     NOT =   ZERO
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *    カーソル指定のない時、次ぎの空白行へ
      *    後画面へ
           IF      SPA-GMN-CUR         =   88  OR  99
               IF      WRK-MAP-MAXPAGE     =   SPA-GMN-PAGE
                   COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                               +   1
               ELSE
                   MOVE    1                   TO  SPA-MAP-IDX
               END-IF
           ELSE
               COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                           +   1
               IF      SPA-MAP-IDX         >   MAX-GMN
                   OR  SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE
                   MOVE    99                  TO  SPA-MAP-IDX
               ELSE
                   IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
                       MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                   END-IF
               END-IF
           END-IF
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
      *
           .
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面数量項目編集処理
      *****************************************************************
       5002-SURYO-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KEI-H 
      *
      *    点数
           MOVE    SPA-GMN-TENSU(WRK-IDX)  TO  WRK-TENSU-X
      *    自費金額入力時のみ、編集
      ***  IF     (SPA-COM-KEIFLG (WRK-IDX)    =   SPACE )
      *        MOVE    SPACE                   TO  WRK-GOKEI-X
      *    ELSE
      *        MOVE    SPA-GMN-KEI (WRK-IDX)   TO  WRK-GOKEI-Z
      ***  END-IF
           IF     (SPA-COM-SRYKBN (WRK-IDX)    =   "95" OR  "96")
               MOVE    SPA-GMN-KEI (WRK-IDX)   TO  WRK-GOKEI-Z
           ELSE
               MOVE    SPACE                   TO  WRK-GOKEI-X
           END-IF
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           IF       WRK-GOKEI-X        =   SPACE
               MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
               MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
           ELSE
               MOVE    WRK-GOKEI-X                 TO  WRK-H-SURYO
           END-IF
           MOVE    SPA-GMN-TENSU(WRK-IDX)   TO  WRK-H-ZAIKEI
      *ver.4.7
           IF     (SPA-GMN-TENSU(WRK-IDX)  =   SPACE )
               IF      SPA-COM-INGAITEN (WRK-IDX) >  ZERO
                   MOVE    SPACE               TO  WRK-TENSU-GX
                   MOVE    SPA-COM-INGAITEN(WRK-IDX)
                                               TO  WRK-TENSU-GZ
                   MOVE    "("                 TO  WRK-TENSU-G1
                   MOVE    ")"                 TO  WRK-TENSU-G2
      *H31.4       MOVE    WRK-TENSU-GX        TO  WRK-H-ZAIKEI
                   MOVE    WRK-TENSU-GX        TO  WRK-H-ZAIKEI-G
                END-IF
           END-IF
      *ver.4.7
      *
           MOVE    WRK-SURYO-HENSYU    TO  J04-SURYO (IDX)
           MOVE    "user-font"         TO  J04-SURYO-STYLE(IDX)
      *
           .
       5002-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX         TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF
      *
           IF      SPA-GMN-CUR         =   10
                                       OR  11
               CONTINUE
           ELSE
               IF      WRK-IDX2            >   MAX-GMN
                   MOVE    00              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    00
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    01
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    02
                   MOVE    "MEISYO"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
               WHEN    03
                   MOVE    "KEI"           TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(4:2)
               WHEN    10
                   MOVE    "HKNCOMBI"      TO  MCP-WIDGET
               WHEN    11
                   MOVE    "HKTKBN"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           IF      SPA-ERRCD           =   "K001"
               MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG
           ELSE
               MOVE    "J04"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING
                                           SPA-AREA
                                           ORCSC96AREA
           END-IF
      *!
      *
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        エラー画面２
               PERFORM 5101-JERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-JERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  JERR2
           INITIALIZE                      JERR2
           MOVE    SPA-ERRCD           TO  JERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  JERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  JERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "JERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "JERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-JERR2-SET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
                   STRING  "診療内容を変更登録します。"
                       "回数も登録されますので取消ができなくなります"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
               WHEN    "0102"
                   MOVE    "外用薬を総量で登録します。"
                                       TO  WRK-JIDMSG
                   MOVE    "診療行為訂正時では、回数が１回となります。"
                                       TO  WRK-JIDMSG(27:)
               WHEN    "0103"
                   STRING  "診療内容を追加登録します。"
                           "回数は会計照会で設定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-JIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JID1-FLG
      *
           MOVE    SPACE               TO  JID1
           INITIALIZE                      JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-J02-SRYYMD      TO  TNS-YUKOSTYMD
           MOVE    SPA-J02-SRYYMD      TO  TNS-YUKOEDYMD
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
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60"
                                           OR  "64"  )  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO )  AND
                  (TNS-SRYCD(1:1)      =   "1"  )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
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
           MOVE    SPA-J02-SRYYMD      TO  TNSP-YUKOSTYMD
           MOVE    SPA-J02-SRYYMD      TO  TNSP-YUKOEDYMD
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
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-ENT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-ENT-NEXT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           IF      WRK-CD-MCNT         =   20
               MOVE    "key2"              TO  WRK-PATH
           ELSE
               MOVE    "key"               TO  WRK-PATH
               COMPUTE IDK                 =   WRK-CD-MCNT   +  1
               MOVE    "%"                 TO  ICD-INPUTCD(IDK:1)
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       960-PTCOM-READ-SEC         SECTION.
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
                   INITIALIZE                  PTCOM-REC
                   MOVE    1                   TO  FLG-PTCOM
               END-IF
           ELSE
               INITIALIZE                  PTCOM-REC
               MOVE    1                   TO  FLG-PTCOM
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       960-PTCOM-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    チェックデータ読込
      *****************************************************************
       950-CHK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHK-REC
               MOVE    ZERO                TO  FLG-CHKMST
           ELSE
               INITIALIZE                      CHK-REC
               MOVE    1                   TO  FLG-CHKMST
           END-IF
      *
           .
       950-CHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       975-INPUTSET-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       975-INPSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
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
       940-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
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
      *****************************************************************
      *    診療会計附加マスター次読込
      *****************************************************************
       910-SRYACCTPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       910-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
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
       990-DBCLOSE-SEC         SECTION.
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
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"         TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
