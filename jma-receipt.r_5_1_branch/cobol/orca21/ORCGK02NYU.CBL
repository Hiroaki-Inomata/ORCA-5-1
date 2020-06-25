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
       PROGRAM-ID.                 ORCGK02NYU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為入力（入院）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.02     NACL-多々納  02/12/02  食事追加
      * 01.00.03     NACL-多々納  02/12/17  算定チェック修正
      * 01.00.04     NACL-多々納  03/02/19  約束セット名称編集修正
      * 01.00.05     NACL-多々納  03/03/05  排他制御処理追加
      * 01.00.06     NACL-多々納  03/03/18  中途終了時修正
      * 01.00.07     NACL-多々納  03/04/11  科変更追加、履歴に科表示他
      * 01.00.08     NACL-多々納  03/05/29  入院アフターケアエラー
      * 01.00.09     NACL-多々納  03/06/09  入院日指定修正
      * 01.00.10     NACL-多々納  03/07/09  時間外区分設定修正
      * 01.00.11     NACL-多々納  03/08/01  数量入力判定追加
      * 01.00.12     NACL-多々納  04/02/20  セット展開変更
      * 01.00.13     NACL-多々納  05/01/20  ヘルプ追加
      * 01.00.14     NACL-多々納  05/02/20  入院指示せん発行追加他
      * 01.00.15     NACL-多々納  05/11/25  MONFUNC 対応
      * 03.02.01     NACL-多々納  06/08/29  平成１８年１０月改正対応
      * 03.04.00     NACL-多々納  06/12/01  自賠責健保準拠追加
      * 03.04.01     NACL-多々納  06/12/14  薬剤附加チェック追加
      * 03.04.02     NACL-多々納  07/01/30  排他中処理追加
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 03.05.00     NACL-多々納  07/05/22  再印刷処理追加
      * 03.05.01     NACL-多々納  07/07/19  "0085XXXXX"コメント対応
      * 04.01.00     NACL-多々納  07/10/XX  テーブル４００対応
      * 04.01.00     NACL-多々納  07/10/22  公務災害対応
      * 04.01.00     NACL-多々納  07/10/XX  悪性腫瘍検査対応
      * 04.01.00     NACL-多々納  07/11/21  器材商品名表示対応
      * 04.01.00     NACL-多々納  07/12/XX  患者禁忌薬剤対応
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      * 04.03.00     NACL-多々納  08/08/01  保険組合せ期間チェック追加
      * 04.03.00     NACL-多々納  08/09/03  投薬帳票印刷対応
      * 04.03.00     NACL-多々納  08/10/09  ユーザプログラム対応
      * 04.04.00     NACL-多々納  08/11/28  処理共通対応
      * 04.05.00     NACL-多々納  09/05/25  回数複写追加
      * 04.05.00     NACL-多々納  09/06/XX  数量小数５桁対応
      * 04.05.00     NACL-多々納  09/08/11  退職国保年齢チェック
      * 04.05.00     NACL-多々納  09/09/16  "0086XXXXX"コメント対応
      * 04.05.00     NACL-多々納  10/01/XX  治験対応
      * 04.06.00     NACL-多々納  10/08/12  診療科変更対応
      * 04.07.00     NACL-多々納  11/09/XX  院外処方点数表示対応
      * 04.07.00     NACL-多々納  11/10/XX  同日再入院対応
      * 04.07.00     NACL-多々納  12/01/05  クライアント印刷対応
      * 04.07.00     NACL-多々納  12/08/01  背景色他対応
      * 04.07.00     NACL-多々納  12/10/10  照会からの遷移対応
      * 04.08.00     NACL-多々納  14/02/XX  行挿入・カーソル移動位置変更
      * 04.08.00     NACL-多々納  14/05/27  一時ファイル変更
      * 04.08.00     NACL-多々納  14/08/11  クライアント印刷変更
      * 04.08.00     NACL-多々納  14/10/23  連続挿入対応
      * 04.08.00     NACL-多々納  17/01/17  翌月入院料チェック対応
      * 04.08.00     NACL-多々納  17/05/XX  公費期間表示、メモ対応
      * 04.08.00     NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      * 04.08.00     NACL-多々納  18/09/XX  選択式コメント対応
      * 05.00.00     NMED-多々納  20/01/XX  訂正時の患者登録更新後対応
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０１年４月改正対応
      *****************************************************************
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
      *    パラメタデータ
      *    SELECT  PARAK02-FILE    ASSIGN  FILE-NAME22
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA2.
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
      *    画面スパ領域パラメタデータ
      *FD  PARAK02-FILE.
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"      REPLACING
      *                              //SPA-// BY //PARA-SPA-//.
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(1500).
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "K02SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME22.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "K01PARA2".
      *    03  FILE-HOSPNUM22      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID22       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    画面Ｋ０８１スパ領域
           COPY    "K081SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-PARA2           PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      **   03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-GMN             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-ALLFLG          PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SYORI           PIC X(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-OK3             PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    初期画面かの判定
           03  FLG-SYOKI           PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
      *
      *    診察料算定済み
           03  FLG-SINSATU          PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
      *    算定履歴有無
           03  FLG-JYURR-ARI       PIC 9(01).
      *    時間外加算用
           03  FLG-DELFLG          PIC 9(01).
           03  FLG-ZAIEND          PIC 9(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SET-HEN         PIC 9(01).
      *    剤チェック対象外
           03  FLG-TAISYO          PIC 9(01).
      *    回数入力なし
           03  FLG-KAISU          PIC 9(01).
      *
           03  FLG-PARASET         PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-KUHAKU          PIC 9(01).
      *
           03  FLG-CHK2            PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-NYU-ARI         PIC 9(01).
           03  FLG-ZAIBETU         PIC 9(01).
      *
           03  FLG-K09TUIKA           PIC 9(01).
      *    入力なし
           03  FLG-INPUT-NASI      PIC 9(01).
      *
           03  FLG-HEN-CHK         PIC 9(01).
      *
           03  FLG-IN1             PIC 9(01).
      *
           03  FLG-SONOTA           PIC 9(01).
           03  FLG-NAIFUTAN         PIC 9(01).
      *
           03  FLG-RIHTEI           PIC 9(01).
      *
           03  FLG-TEISEI           PIC 9(01).
           03  FLG-MOD-FLG           PIC 9(01).
      *    画面変更
           03  FLG-HENKOU            PIC 9(01).
      *    入院指示せん
           03  FLG-SIJISEN-OK        PIC 9(01).
      *    入院指示せん
           03  FLG-SIJISEN           PIC 9(01).
      *    初期
           03  FLG-INIT              PIC 9(01).
           03  FLG-MAE-TENSU         PIC 9(01).
      *
           03  FLG-WKSRYACT-ARI        PIC 9(01).
      *
           03  FLG-KENSADEL            PIC 9(01).
      *
           03  FLG-HOUKATU-ZAI         PIC 9(01).
      *
           03  FLG-SYOKAI           PIC 9(01).
           03  FLG-RRK-ARI          PIC 9(01).
      *
           03  FLG-COPY                PIC 9(01).
           03  FLG-COPYOK              PIC 9(01).
      *
           03  FLG-DOUNYUIN            PIC 9(01).
           03  FLG-NYUNEXT             PIC 9(01).
      *    クライアント印刷有
      **   03  FLG-CLIENT-OK           PIC 9(01).
      *
      *    スクロール背景
           03  FLG-BGCOLORS            PIC 9(01).
      *ver.4.8
           03  FLG-POPINI              PIC 9(01).
      *    悪性区分前
           03  FLG-MAE-AKUSEI          PIC 9(01).
      *
      *!!!!!!!!!!!!!!!!!!
      *    連続行挿入
           03  FLG-INPUT-NASI2         PIC 9(01).
           03  FLG-F6                  PIC 9(01).
           03  FLG-F7                  PIC 9(01).
           03  FLG-INSEND              PIC 9(01).
      *!!!!!!!!!!!!!!!!!!
      *H29.4
           03  FLG-KEIFLG              PIC 9(01).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
      *
           03  IDX-ERR             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-SIN2            PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-KAI2            PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-K4              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
      *    03  IDX-Y2              PIC 9(02).
      *    03  IDX-Y3              PIC 9(02).
      *
           03  IDX-YAK             PIC 9(04).
      *
           03  IDX-K98             PIC 9(04).
      *
           03  IDX-HKN             PIC 9(04).
      *
           03  IDN1                PIC 9(04).
      *
           03  IDX-GG              PIC 9(04).
           03  WRK-MAX             PIC 9(04).
           03  WRK-STR             PIC 9(04).
           03  IDX-G               PIC 9(04).
      *
           03  IDX-SET             PIC 9(04).
           03  IDX-TEI             PIC 9(04).
      *
           03  IDX-NUM             PIC 9(06).
           03  IDY2                PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
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
           03  WRK-TOUYMD.
               05  WRK-TOUYY       PIC 9(04).
               05  WRK-TOUMM       PIC 9(02).
               05  WRK-TOUDD       PIC 9(02).
      *
           03  WRK-FREE            PIC 9(04).
           03  WRK-IDX             PIC 9(04).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(04).
           03  WRK-IDX3            PIC 9(04).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(04).
      *    年齢判定用
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
           03  WRK-NYUYOJI         PIC 9(04).
      *
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-GOKEI-X3.
               05  WRK-GOKEI-Z3    PIC ---,---,---.
           03  WRK-NOZ             PIC ZZ9.
      *ver.4.7
           03  WRK-TENSU-GX.
               05  WRK-TENSU-G1    PIC X(01).
               05  WRK-TENSU-GZ    PIC ZZZZZZ.
               05  WRK-TENSU-G2    PIC X(01).
      *
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(03).
      *
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-KAISU           PIC 9(03).
           03  WRK-KAISU-GOK       PIC 9(04).
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-DOSELNUM            PIC 9(04).
           03  WRK-DOSELNUM2           PIC 9(04).
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-SELNUM2             PIC 9(04).
      *
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-BUNGSU          PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-GOKEI-9         PIC 9(10).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-PARAEDABAN      PIC 9(04).
      *     一部負担金計算用
           03  WRK-S01-TENSU       PIC 9(08).
           03  WRK-ACCT-KAISU      PIC 9(03).
      *
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-SRYCD           PIC X(09).
           03  IDX-S               PIC 9(04).
      *    時間外フラグ
           03  WRK-TIMEFLG         PIC X(01).
      *    算定回数
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
           03  WRK-FLG-IN4             PIC X(01).
      *
           03  WRK-KINKIFLG            PIC X(01).
           03  WRK-PTKINKI-ARI         PIC 9(01).
      *
           03  WRK-KIDMSG          PIC X(80).
           03  WRK-KID-SRYCD       PIC X(09).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
      *    労災保険用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-EN              PIC 9(08).
      *    訂正時の点数判定
           03  WRK-MAE-TENSU           PIC 9(08).
      *
           03  WRK-KEI-ERRCD           PIC X(04).
           03  WRK-KEI-ERRMSG2         PIC X(80).
           03  WRK-ERR-ERRCD           PIC X(04).
           03  WRK-ERR-ERRMSG2         PIC X(80).
      *
           03  WRK-KEI-IDX             PIC 9(04).
      *    老人年齢計算領域
           03  WRK-NENREI.
               05  WRK-NENREI-YY         PIC 9(03).
               05  WRK-NENREI-MM         PIC 9(02).
               05  WRK-NENREI-DD         PIC 9(02).
      *    老人フラグ
           03  WRK-ROUJIN              PIC 9(01).
      *
      *    入力時表示用
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(54).
           03  FLG-IN-ATAI             PIC 9(01).
      *
      *    入力位置判定用
           03  WRK-IN-SURYO        PIC 9(02).
           03  WRK-IN-IDX          PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
           03  WRK-MAX-IDX         PIC 9(04).
      *
           03  WRK-WKSRY-HKNCOMBI      PIC 9(04).
      *
           03  WRK-ERRCD           PIC X(04).
           03  WRK-ERRCD1          PIC X(04).
           03  WRK-ERRCD2          PIC X(04).
           03  WRK-KIDCD           PIC X(04).
      *    パス名
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-SAKIPG          PIC X(08).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
      *    ＤＯ選択編集
           03  WRK-ZZ              PIC ZZZZ.
      *    保険名称編集
           03  WRK-HKNCOMBI        PIC X(50).
      *    保険組合せ保存
           03  WRK-MAE-HKNCOMBI    PIC X(04).
      *
      *    特定疾患自動算定チェック用
           03  FLG-KYUJITU         PIC 9(01).
           03  WRK-KEIYMD          PIC X(08).
      *
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
      *R02.    05  WRK-MEISYO              PIC X(78).
               05  WRK-MEISYO              PIC X(138).
           03  IDX-MEI                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
               05  WRK-HZN-ATAI5           PIC X(10).
      *    老人年齢
           03  WRK-ROU-NEN             PIC 9(02).
      *    入院日
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
      *   入院回数・日数
           03  WRK-NYUIN-AREA.
               05  WRK-NYU-KAI        PIC 9(03).
               05  WRK-NYUINDAY       PIC 9(01)    OCCURS  31.
           03  WRK-NYUIN-CHK-AREA.
               05  WRK-NYUIN-KAI       PIC 9(03).
           03  IDX-N2                  PIC 9(02).
           03  WRK-NYU-KAISU           PIC 9(03).
      *    消炎鎮痛回数
           03  WRK-SYOEN-CNT           PIC 9(03).
      *    入退院日
           03  WRK-NYUINYMD            PIC X(08).
           03  WRK-TAIINYMD            PIC X(08).
      *    診療選択より
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBINUM         PIC X(04).
      *
      *    コラムリスト位置
           03  WRK-K02-ROW             PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
           03  WRK-CHUSYA-PG           PIC X(20).
           03  WRK-SIJISEN-PG          PIC X(20).
      *    急性発生日
           03  WRK-KYUSEI-YMD          PIC X(08).
           03  WRK-KYUSEI-END          PIC X(08).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
      *    P97検索名
           03  WRK-P97-NAME            PIC X(22).
           03  WRK-GMN-PTID            PIC 9(10).
      *@
      *    今回リハビリ発生日
           03  WRK-KYUSEI-INP-G.
               05  WRK-KYUSEI-IN-G         OCCURS   7.
                   07  WRK-KYUSEI-ST       PIC X(01).
                   07  WRK-KYUSEI-ED       PIC X(01).
               05  WRK-TEIGEN-IN           PIC X(01).
      *!
           03  WRK-HOSPNUM             PIC 9(02).
           03  WRK-GRPHOSPUSERID       PIC X(64).
           03  WRK-MACHIPG             PIC X(08).
      *!
           03  WRK-TUKIJGNFLG          PIC X(01).
           03  WRK-TUKIJGNFLG1         PIC X(01).
      *
      *!!!!!!!!!!!!!!!!!!
      *    挿入行数
           03  WRK-INSCNT              PIC 9(02).
           03  WRK-GYO                 PIC 9(04).
      *    改頁行数
      **** 03  WRK-PAGE-LINE           PIC 9(02).
      *!!!!!!!!!!!!!!!!!!
      *H29.1
      *    翌月入院料チェック
           03  FLG-NEXTTUKI            PIC 9(01).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
      *H30.9
           03  FLG-AUTOERR             PIC 9(01).
           03  IDX-SELCOM              PIC 9(04).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
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
      *ver.4.7
           03  WRK-MAP-CUR4        PIC 9(04).
           03  FLG-MAP-CUR4        PIC 9(01).
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO        PIC X(11).
               05  WRK-H-TANINAME     PIC X(04).
               05  WRK-H-F1           PIC X(01).
               05  WRK-H-ZAIKEI       PIC X(14).
               05  WRK-H-KEI          PIC X(08).
      *
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC Z(08).
           03  WRK-KEI-X2.
               05  WRK-KEI-Z2      PIC -(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-KAKERU          PIC X(03).
      *
        01  WRK-HENSYU-AREA.
      *    ページ表示
           03  WRK-HEN-PAGE.
               05  WRK-HEN1               PIC X(02).
               05  WRK-HEN-PAGE1          PIC ZZZ.
               05  WRK-HEN2               PIC X(01).
               05  WRK-HEN-PAGE2          PIC ZZ.
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
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-SUR-MCNT        PIC 9(02).
           03  WRK-MCNT1           PIC 9(02).
           03  WRK-MCNT2           PIC 9(02).
           03  WRK-MCNT3           PIC 9(02).
           03  WRK-MCNT4           PIC 9(02).
           03  WRK-MCNT5           PIC 9(02).
           03  WRK-CD              PIC X(54).
           03  WRK-MOJI1           PIC X(10).
           03  WRK-MOJI2           PIC X(10).
           03  WRK-MOJI3           PIC X(10).
           03  WRK-MOJI4           PIC X(10).
           03  WRK-MOJI5           PIC X(10).
      *
      *    自動加算テーブル
       01  WRK-KASAN-AREA.
           03  WRK-KASAN-TBL                       OCCURS   60.
               05  WRK-KSN-SRYSYUKBN       PIC X(03).
               05  WRK-KSN-SRYKBN          PIC X(02).
               05  WRK-KSN-SRYCD           PIC X(09).
               05  WRK-KSN-ATAI-G.
                   07  WRK-KSN-ATAI        PIC X(08)   OCCURS  5.
               05  WRK-KSN-TENSU           PIC 9(08).
               05  WRK-KSN-SURYO           PIC 9(05)V9(05).
               05  WRK-KSN-ZAINUM          PIC 9(08).
               05  WRK-KSN-ZAIEND          PIC X(01).
               05  WRK-KSN-INPUTCD         PIC X(54).
               05  WRK-KSN-AUTOKBN         PIC X(01).
           03  WRK-KSN-IDX             PIC 9(04).
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    画面ＳＰＡ領域ワーク
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
       01 WRK-HOZ-IDX                  PIC 9(04).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *    画面スパ領域ワーク
           COPY    "K02SCR-SPA"      REPLACING
                                     //SPA-// BY //HZN-SPA-//.
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA"   REPLACING 
                                     //SPA-// BY //HZN-SPA-//.
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA"   REPLACING 
                                     //SPA-// BY //WRK-SPA-//.
      *
       01  WRK-SYSTIME               PIC 9(08).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *画面最大行
      *01  MAX-GMN                 PIC 9(04)   VALUE   24.
      *ver.4.7
       01  MAX-GMN                 PIC 9(04)   VALUE   400.
      *画面ページ数（処理最大行÷画面最大行）
      *01  MAX-PAGE                PIC 9(04)   VALUE   16.
      *01  MAX-PAGE2               PIC 9(04)   VALUE   17.
       01  MAX-PAGE                PIC 9(04)   VALUE   1.
       01  MAX-PAGE2               PIC 9(04)   VALUE   1.
      *
      *入力文字数
       01  CONS-INPUT              PIC 9(04)   VALUE   54.
      *
      *改頁行数
       01  PAGE-LINE               PIC 9(02)   VALUE   28.
      *
      *
      *PandaTable色
      *青
       01  CL-BLUE                 PIC X(07)   VALUE   "#0000EE".
      *赤
       01  CL-RED                  PIC X(07)   VALUE   "#FF0000".
      *クロ
       01  CL-BLACK                PIC X(07)   VALUE   "#000000".
      *白
       01  CL-WHITE                PIC X(07)   VALUE   "#FFFFFF".
      *緑
       01  CL-GREEN                PIC X(07)   VALUE   "#00B000".
      *紫
      *01  CL-GREEN                PIC X(07)   VALUE   "#8000FF".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *D   COPY    "CPSYSKANRI.INC".
      *
      *    COPY    "CPSK1005.INC".
      *
      *    COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *    帳票ＰＧ
           COPY    "CPSK1034.INC".
      *
      *    入院指示せん設定情報
           COPY  "CPSK5007.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
      *01  PTNUM-REC.
      *    COPY    "CPPTNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ−
      *01  SRYACT-REC.
      *    COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
      *01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *
      *    チェックマスタ−
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *
      *    入力セットコード
      *01  INPUTSET-REC.
      *    COPY    "CPINPUTSET.INC".
      *
      *    受診履歴
      *01  JYURRK-REC.
      *    COPY    "CPJYURRK.INC".
      *
      *    算定履歴
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
      *
      *    患者入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *    患者入院履歴ワーク
      *01  HZN-PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC"  REPLACING
      *                        //PTNYUINRRK-// BY //HZN-PTNYRRK-//.
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *    収納マスタワーク
      *01  HZN-SYUNOU-REC.
      *    COPY    "CPSYUNOU.INC"  REPLACING
      *                              //SYU-// BY //HZN-SYU-//.
      *
      *    保険組み合わせ
      *01  HKNCOMBI-REC.
      *    COPY    "CPHKNCOMBI.INC".
      *
      *    患者病名
      *01  PTBYOMEI-REC.
      *    COPY    "CPPTBYOMEI.INC".
      *
      *    相互作用マスタ
      *01  INTERACT-REC.
      *    COPY    "CPINTERACT.INC".
      *
      *!!!
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *!!!
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *D  COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金額計算用パラメタ
      *D   COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      *D   COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    投与量等チェックパラメタ
      *D   COPY    "CPORCSC98.INC".
      *
      *    診察料自動発生パラメタ
           COPY    "CPORCSC10S.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    入院指示せんオーダ作成サブ
           COPY    "CPORCSORDERPRT.INC".
      *    入院処方箋発行サブ
           COPY    "CPORCHC501.INC".
      *    注射処方箋発行サブ
           COPY    "CPORCHC502.INC".
      *    入院指示箋発行サブ
           COPY    "CPORCHC503.INC".
      *    内分泌負荷試験更新パラメタ
      **   COPY    "CPORCSSANFUKA.INC".
      *
      *    禁忌薬剤チェックパラメタ
           COPY    "CPORCSCKNKCHK.INC".
      *
      *   算定履歴更新サブ
      *    COPY    "CPORCSSANTEI.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    入院履歴編集サブパラメタ
           COPY    "CPORCSCNYUHEN.INC".
      *
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    COMMONSPA初期編集
           COPY    "CPORCSCOMMON.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *
      *    患者情報通知パラメタ
           COPY    "CPORCSPOPUPSET.INC".
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
           COPY    "ORCA21SCRAREA.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-END
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
      *v4.7
      *    頁行数
           IF     (SPA-GMLINCNT(1:2)   NOT NUMERIC )
             OR   (SPA-GMLINCNT        =    ZERO)
               MOVE    PAGE-LINE            TO  SPA-GMLINCNT
           END-IF
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-K02SPA-SET-SEC
      *    画面制御
           MOVE    SPACE           TO  WRK-PUTTYPE
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
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
      *    IF     (FLG-END2            =   ZERO )
      *      AND  (SPA-ERRCD       NOT =   "1999")
      *        PERFORM 1003-K02SPA-OUT-SEC
      *    END-IF
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    "K02N"          TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "KERR"
                                       OR  "KERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "K02N"              TO  MCP-WINDOW
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
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-K02SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF     SPA-HOSPNUM  NOT NUMERIC
               GO   TO  1002-K02SPA-SET-EXT
           END-IF  
      *
      *    TBL_PARA を使用する
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
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
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC       TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    ZERO                TO  SPA-GMN-MAX
           ELSE
      *
      *        行位置とカーソル位置の決定
               PERFORM 10021-K02SPA-INIT-SET-SEC
           END-IF
      *
           .
       1002-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       10021-K02SPA-INIT-SET-SEC             SECTION.
      *
      *    行位置とカーソル位置の決定
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  WRK-MAP-MAX
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
      *            画面表示の最終行（次頁用）
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-INPUTCD(WRK-IDX)  NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
      *********    ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-IDX         TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           .
       1002-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-K02SPA-OUT-SEC             SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(IDX)   =   SPACE )
               AND     (IDX           >   SPA-GMN-MAX  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-TBL (IDX)   TO  PARA-DATA-REC
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K02 PARA INSERT ERR:"  MCP-RC
                           ",KEY:" PARA-KEY
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       1003-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-GMN
           MOVE    SPACE               TO  WRK-KID-SRYCD
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        画面ＳＰＡ読み込み
               PERFORM 1002-K02SPA-SET-SEC
      *
      *        元画面へ戻す
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               IF      WRK-MOTOPG          =   "Y01"  OR  "U01"
                                                      OR  "P97"
                                                      OR  "KA02"
                                                      OR  "XF01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-K02
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
      *        テーブル最大行
               MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *!
               CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG          =   "Y01"
                                           OR  "KA02"
                   IF      SPA-PTID            =   ZERO
                       INITIALIZE              SPA-K01SET-AREA
                       MOVE    SPA-SYOHOKBN    TO  SPA-INGAIKBN
                       MOVE    ZERO            TO  SPA-GMN-CUR
                   END-IF
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *H29.5
      *        メモ画面
               IF      WRK-MOTOPG          =   "XF01"
      *            メモ登録有無
                   PERFORM 41021-PTMEMOINF-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
           END-IF
      *!
           CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "K99"
                   IF      SPA-K99-DATA    NOT =   SPACE
                        MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
                                                   TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                       MOVE    SPA-K99-DATA        TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
      *
      *                入力コード・名称チェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *                画面ＳＰＡチェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "K98"
      *@
      *            悪性腫瘍の検査
                   IF     SPA-AKUSEI-FLG       =   1
                       PERFORM 30112-AKUSEI-COMMENT-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *TEST
      *H30.9
      *            選択式コメント
                   IF      SPA-SELCOM-FLG      =   1
                       PERFORM 30113-SELCOMMNT-COMMENT-SEC
                   ELSE
      *
      *
                   IF      SPA-K98-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-K98-SET-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
                       MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                       MOVE    2                   TO  FLG-GMN
      *H30.9
                   END-IF
      *
                   END-IF
      *
                   END-IF
               WHEN    "K04"
      *            算定処理より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K021"
      *             禁忌一覧 ＯＫの時、更新処理へ
                   IF     (SPA-K021-SYORI      =   2  )  AND
                          (SPA-K021-CHK        =   1  )
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       EVALUATE    SPA-K021-SAKI
                           WHEN    "K03"
      *                        更新処理
                               PERFORM 41002-KOUSIN-SYORI-SEC
                           WHEN    "C02"
      *                        病名登録遷移処理
                               PERFORM 4401-C02SET-SEC
                       END-EVALUATE
                   END-IF
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K022"
      *            禁忌画面より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K023"
      *            入力コード設定
                   PERFORM 30014-K023-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KID2"
      *            確認画面２より
                   PERFORM 30011-KID2-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K09"
      *            ＤＯ選択画面より
                   PERFORM 3013-K09-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KHELP"
      *            ヘルプ画面より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K081"
      *            指示せん発行画面
                   PERFORM 3014-K081-SET-SEC
                   MOVE    2                   TO  FLG-GMN
      *
               WHEN    "M01"
      *            業務選択画面より
                   PERFORM 30010-INIT-GMN-SEC
               WHEN    "XD01"
      *             ユーザプログラム実行画面より
                   PERFORM 30010-INIT-GMN-SEC
      *ver.4.7
               WHEN    "Q02"
               WHEN    "Q02A"
               WHEN    "Q02B"
      *            照会より
                   IF      LNK-KYOUTU      NOT =   SPACE
                       MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
                       PERFORM 30010-INIT-GMN-SEC
                       MOVE    WRK-SPA-PTNUM       TO  K02N-PTNUM
                   ELSE
                       MOVE    SPA-PTNUM           TO  K02N-PTNUM
                   END-IF
                   PERFORM 41010-PTNUM-SYORI-SEC
               WHEN    OTHER
      *            外部・患者決定
                   PERFORM 3000-SCREEN-SYORI-SEC
           END-EVALUATE
      *
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
               ELSE
                   IF      SPA-KIDCD       NOT =   SPACE
      *                画面編集
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 520-KIDSET-SEC
                   END-IF
               END-IF
      *!!
           ELSE
               IF     (FLG-END2        =   1      )
                  OR  (SPA-ERRCD       =   "1999" )
                   MOVE    "1999"          TO  SPA-ERRCD
                   MOVE    "E"             TO  SPA-K01-YOBI(1:1)
               END-IF
      *!!
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者設定初期画面処理
      *****************************************************************
       3000-SCREEN-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
           MOVE    ZERO                TO  FLG-SYOKAI
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "K01"
      *            診療選択より
                   IF      SPA-ERRMSG(1:2)    =   "NO"
      *                戻る
                       MOVE    ZERO               TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       PERFORM 3003-GMN-HEN-SEC
                       IF      SPA-GMN-PTID   NOT =   ZERO
      *                    排他制御処理
                           PERFORM 990-LOCK-IN-SEC
                       END-IF
                   ELSE
                       MOVE    ZERO               TO  SPA-SAISIN-CHK
                       MOVE    2                  TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       IF      SPA-SYORIFLG        =   ZERO
                          AND  SPA-WKSRY-HKNCOMBI  =   ZERO
                          PERFORM 3009-SAIHEN-SEC
                       END-IF
                   END-IF
               WHEN    "K02"
      *            外来診療行為より
                   IF      SPA-PTNUM           =   SPACE
                       PERFORM 30010-INIT-GMN-SEC
                   ELSE
                       MOVE    SPA-PTNUM           TO  K02N-PTNUM
                       PERFORM 41010-PTNUM-SYORI-SEC
                   END-IF
               WHEN    "P02"
      *            患者登録より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO                TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF      SPA-ERRMSG(1:2)     =   "NO"
      *                戻る
                       PERFORM 3003-GMN-HEN-SEC
                   ELSE
                       IF      SPA-PTNUM       =   SPACE
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           IF      SPA-SYORIFLG        =   ZERO
                               PERFORM 30011-INIT-HEN-SEC
                           ELSE
                               PERFORM 3003-GMN-HEN-SEC
      *!!!
      *R02.1                   患者登録変更チェック
                               PERFORM 30044-PTINF-P02CHEK-SEC
                           END-IF
                       END-IF
                   END-IF
               WHEN    "U02"
      *            受付より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF     (SPA-PTNUM       NOT =   SPACE    ) AND
                          (SPA-GMN-PTNUM   NOT =   SPA-PTNUM)
                       PERFORM 3009-SAIHEN-SEC
                   ELSE
                       IF     (SPA-PTNUM       =   SPACE )  AND
                              (SPA-GMN-PTNUM   =   SPACE )
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           PERFORM 3003-GMN-HEN-SEC
                       END-IF
                   END-IF
      *
               WHEN    "K08"
      *            登録後クリア画面の表示
                   IF      SPA-PTNUM          =   SPACE
                       PERFORM 30010-INIT-GMN-SEC
                   ELSE
      *
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
      *            特定疾患判定のクリア
                   MOVE    ZERO            TO  SPA-TOKUTEIFLG
                   MOVE    ZERO            TO  SPA-CHOZAI-CHK
      *            診療行為、計計算処理、編集
      *            MOVE    "2"             TO  FLG-TOROKU
                   MOVE    SPACE           TO  FLG-TOROKU
                   PERFORM 510-TBLHEN-SEC
                   MOVE    SPACE           TO  FLG-TOROKU
      *
                   END-IF
      *
               WHEN    "K10"
                   IF      SPA-K10-SELNUM      =   ZERO
                       MOVE    2                   TO  FLG-SYOKAI
                   ELSE
                       PERFORM 30091-SAIHEN2-SEC
                   END-IF
      *!!!
      *            ＤＢエラー表示
                   IF      SPA-ERRCD       =   "1999"
                                           OR  "9999"
                       MOVE    01              TO  SPA-GMN-CUR
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 510-ERRSET-SEC
                   END-IF
      *
               WHEN    OTHER
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   PERFORM 3003-GMN-HEN-SEC
                   IF      SPA-MOTOPG      =   "J02"
      *                会計照会からの戻りの時、受診履歴判定
                       IF      SPA-PTID    NOT =   ZERO
                           PERFORM 30041-JYURRK-J02CHK-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      FLG-SYOKAI          =   2
               CONTINUE
           ELSE
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    1               TO  SPA-MAP-IDX
           END-IF
      *
           .
       3000-SCREEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    会計照会変更チェック処理
      *****************************************************************
       30041-JYURRK-J02CHK-SEC              SECTION.
      *
           MOVE    SPA-K02             TO  HZN-SPA-K02
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "N"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       HZN-SPA-K02
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX1 )
               IF      (SPA-NAI-RRKYMD  (IDX)  =   HZN-SPA-NAI-RRKYMD
                                                                 (IDX))
               AND    (SPA-NAI-RENNUM   (IDX)  =   HZN-SPA-NAI-RENNUM
                                                                 (IDX))
               AND    (SPA-NAI-RRK-SRYKA(IDX)  =   HZN-SPA-NAI-RRK-SRYKA
                                                                 (IDX))
               AND    (SPA-NAI-RRK-HKNCOMBI(IDX)
                                           =   HZN-SPA-NAI-RRK-HKNCOMBI
                                                                  (IDX))
                   CONTINUE
               ELSE
                   MOVE    "K178"          TO  SPA-ERRCD
               END-IF
               IF      SPA-SYORIFLG        =   1
                   IF      (SPA-SRYYMD     =   HZN-SPA-NAI-RRKYMD(IDX))
                   AND     (SPA-OLD-SRYKA      =   HZN-SPA-NAI-RRK-SRYKA
                                                                 (IDX))
                   AND     (SPA-OLD-HKNCOMBI
                                           =   HZN-SPA-NAI-RRK-HKNCOMBI
                                                                 (IDX) )
                   AND     (SPA-RENNUM     =   HZN-SPA-NAI-RENNUM(IDX))
                   AND     (SPA-DOUJI-RENNUM
                                           =   HZN-SPA-NAI-DOUJI-RENNUM
                                                                 (IDX))
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-GMN-MAX1    NOT =   HZN-SPA-GMN-MAX1
               MOVE    "K178"              TO  SPA-ERRCD
               MOVE    90                  TO  SPA-GMN-CUR
           END-IF
           MOVE    ZERO                TO  SPA-NAI-JYURRKFLG
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-NAI-JYURRKFLG
           END-IF
           IF     (SPA-SYORIFLG        =   1  )  AND
                  (FLG-OK              =   ZERO)
               MOVE    "0178"              TO  SPA-ERRCD
               MOVE    90                  TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-NAI-JYURRKFLG
           END-IF
      *H26.12
      *    警告をエラーにする
           IF      SPA-NAI-JYURRKFLG   =   2
               MOVE    "0178"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-JYURRKFLG
           END-IF
           .
       30041-JYURRK-J02CHK-EXT.
           EXIT.
      *!!!!
      *R02.1
      *****************************************************************
      *    患者登録変更チェック処理
      *****************************************************************
       30044-PTINF-P02CHEK-SEC         SECTION.
      *
           MOVE    SPA-K02             TO  HZN-SPA-K02
           MOVE    SPA-K01KYOUTU       TO  HZN-SPA-K01KYOUTU
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       WRK-SPA-AREA
                                       HZN-SPA-K01KYOUTU
                                       HZN-SPA-K02
      *
           MOVE    ZERO                TO  FLG-CHK
           IF     (SPA-NAME        NOT =   WRK-SPA-NAME    )
              OR  (SPA-KANANAME    NOT =   WRK-SPA-KANANAME)
              OR  (SPA-BIRTHDAY    NOT =   WRK-SPA-BIRTHDAY)
              OR  (SPA-SEX         NOT =   WRK-SPA-SEX     )
               MOVE    1                   TO  FLG-CHK
           END-IF
      *    保険組合せ
           MOVE    ZERO                TO  FLG-OK
           IF     (HZN-SPA-HKN-MAX NOT =   SPA-HKN-MAX)
               MOVE    1               TO  FLG-OK
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   HZN-SPA-HKN-MAX )
                         OR   (FLG-CHK     =    1   )
                         OR   (FLG-OK      =    1   )
               IF      SPA-GMN-THKNCOMBINUM (IDX)
                                       NOT =   HZN-SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    1                 TO  FLG-OK
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-NAI-PTINFFLG
           IF     (FLG-CHK             =   1    )
              OR  (FLG-OK              =   1    )
      *        患者情報変更あり
               MOVE    "0179"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-PTINFFLG
           END-IF
           INITIALIZE                      HZN-SPA-K02
           INITIALIZE                      HZN-SPA-K01KYOUTU
           INITIALIZE                      WRK-SPA-AREA
           .
       30044-PTINF-P02CHEK-EXT.
           EXIT.
      *!!!
      *
      *****************************************************************
      *    Ｋ０２Ｎ初期画面表示
      *****************************************************************
       30010-INIT-GMN-SEC              SECTION.
      *
      *    一時データ削除
           PERFORM 210-PARA-DEL-SEC
           MOVE    ZERO                TO  FLG-END2
      *
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    0               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       30010-INIT-GMNN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理
      *****************************************************************
       30011-INIT-HEN-SEC              SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
           MOVE    SPA-GMN-HKNCOMBINUM TO  WRK-MAE-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD           =   "9999"
               GO      TO      30011-INIT-HEN-EXT
           END-IF
      *
      *H29.5
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
           MOVE    WRK-DRCD            TO  SPA-DRCD
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-GMN-HKNCOMBINUM
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-HKNCOMBINUM
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      30011-INIT-HEN-EXT
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
      *
           .
       30011-INIT-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＳＰＡのＫ０２を共通ＳＰＡへ
      *****************************************************************
       3003-GMN-HEN-SEC              SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               GO      TO      3003-GMN-HEN-EXT
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
      *    前回患者保存
           MOVE    SPA-PTID            TO  SPA-LAST-PTID
           MOVE    SPA-PTNUM           TO  SPA-LAST-PTNUM
      *
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      3003-GMN-HEN-EXT
           END-IF
      *
      *H29.5
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           .
       3003-GMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理
      *****************************************************************
       3009-SAIHEN-SEC              SECTION.
      *
      *    患者番号変更、Ｋ０１、Ｐ０２よりの時　初画面を編集
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *
      *    患者情報通知判定
           IF      SPA-PTID            =   SPA-GMN-PTID
               MOVE    ZERO            TO  FLG-POPINI
           ELSE
               MOVE    1               TO  FLG-POPINI
           END-IF
      *
      *    初期ＳＰＡ編集処理
           IF     (SPA-MOTOPG          =   "P02"  OR  "U02" )  AND
                  (SPA-PTID        NOT =   SPA-GMN-PTID )
      **       IF      SPA-MOTOPG          =   "P02"  OR  "U02"
      *            MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
      *            MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *            IF      SPA-SRYYMD          =   SPACE
      *                MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *                MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
      **           END-IF
      *        END-IF
      *        患者変更時は、診療日＝システム日付とする
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
      *        初期ＳＰＡ編集処理
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "9"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           ELSE
               IF     (SPA-PTID            =   SPA-GMN-PTID )  AND
                      (SPA-NAI-SRYYMD  NOT =   SPA-SRYYMD   )
                   MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
                   MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
               END-IF
           END-IF
           IF      SPA-PTID            =   ZERO
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD           =   "9999"
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
      *H29.5
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *    診療選択からの時、診療選択分とする
           IF      SPA-MOTOPG          =   "K01"
               MOVE    SPA-DRCD            TO  WRK-DRCD
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
      *        労災アフターケア選択は、入院基本情報の保険に変更
               IF     (SPA-HKNNUM          =   "971" )
                 AND  (SPA-RSI-HKNKBN      =   "3"   )
                   MOVE    SPACE           TO  WRK-HKNCOMBINUM
               END-IF
           ELSE
               MOVE    SPACE               TO  WRK-DRCD
               MOVE    SPACE               TO  WRK-HKNCOMBINUM
               MOVE    SPACE               TO  WRK-SRYKA
           END-IF
      *
      *    入院が存在するかのチェック
           PERFORM 410101-NYUINRRK-CHK-SEC
      *    入外区分
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、外来処理へ
               MOVE    SPACE               TO  WRK-PUTTYPE
               PERFORM 410109-K02IDOU-SEC
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
      *    診療選択からの時、診療選択分とする
           IF      SPA-MOTOPG          =   "K01"
               IF      WRK-HKNCOMBINUM NOT =   SPACE
                   MOVE    WRK-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
               END-IF
               MOVE    WRK-DRCD            TO  SPA-DRCD
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
               MOVE    WRK-SRYKA           TO  SPA-GMN-SRYKA
           END-IF
      *H29.5
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           IF     (SPA-MOTOPG          =   "K01"        )  AND
                  (SPA-PTID            =   SPA-GMN-PTID )
               MOVE    "K01"               TO  LNK-SC95-PG
           ELSE
               MOVE    "K02"               TO  LNK-SC95-PG
               IF     (SPA-MOTOPG          =   "K01"    )  AND
                      (WRK-SRYKA       NOT =   SPACE    )
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  SPA-SRYKA
               END-IF
           END-IF
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *ver.4.8
           IF      FLG-POPINI          =   1
      *        患者情報通知
               PERFORM 41023-POPUP-SYORI-SEC
           END-IF
      *
      *
           IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
               PERFORM                 310-SPA-SET-SEC
               MOVE   "1006"           TO  SPA-ERRCD 
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    00              TO  SPA-GMN-CUR
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
           MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *
           .
       3009-SAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理（Ｋ１０より）
      *****************************************************************
       30091-SAIHEN2-SEC              SECTION.
      *
      *    中途終了分よりの時　初画面を編集
           INITIALIZE                  SPA-SCR-REC
      *
      *    ワーク診療行為解除
           MOVE    ZERO            TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    患者情報通知判定
           IF      SPA-PTID            =   SPA-GMN-PTID
               MOVE    ZERO            TO  FLG-POPINI
           ELSE
               MOVE    1               TO  FLG-POPINI
           END-IF
      *
      *
      *    初期ＳＰＡ編集処理
           IF     (SPA-SRYYMD      NOT =   SPA-GMN-SRYYMD )  OR
                  (SPA-PTID        NOT =   SPA-GMN-PTID   )
      *        選択された科・保険
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
      *        入院が存在するかのチェック
               MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
               IF      SPA-WKSRY-DOUKBN    =   "1"
                   MOVE    1               TO  FLG-DOUNYUIN
               ELSE
                   MOVE    ZERO            TO  FLG-DOUNYUIN
               END-IF
      *
               INITIALIZE                  SPA-K02
      *
               PERFORM 410101-NYUINRRK-CHK-SEC
      *        入外区分
               MOVE    "1"                 TO  SPA-NYUGAIKBN
               MOVE    WRK-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
               MOVE    SPA-K01KYOUTU       TO  WRK-SPA-K01KYOUTU
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "9"                 TO  LNK-SC95-KBN
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
      *        入退院日
      *        MOVE    WRK-NYUINYMD        TO  SPA-K02N-NYUINYMD
      *        MOVE    WRK-TAIINYMD        TO  SPA-K02N-TAIINYMD
      *        入院内容再編集
               MOVE    WRK-SPA-K02N-DATA-REC   TO  SPA-K02N-DATA-REC
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    "5"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           ELSE
      *
      *        中途終了分のみ
               INITIALIZE                  ORCSC95AREA
               MOVE    "8"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           END-IF
      *
           IF      FLG-POPINI          =   1
      *        患者情報通知
               PERFORM 41023-POPUP-SYORI-SEC
           END-IF
      *
      *!   IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
      *        PERFORM                 310-SPA-SET-SEC
      *        MOVE   "1006"           TO  SPA-ERRCD 
      *        MOVE    1               TO  SPA-GMN-PAGE
      *        MOVE    00              TO  SPA-GMN-CUR
      *        MOVE    3               TO  SPA-HKN-ERR
      *        GO      TO      30091-SAIHEN2-EXT
      *!   END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
           .
       30091-SAIHEN2-EXT.
           EXIT.
      *
      *H29.5
      *****************************************************************
      *    メモ登録有無処理
      *****************************************************************
       41021-PTMEMOINF-SEC         SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
      *D   MOVE    2                   TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-SYSYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
                   MOVE    ZERO                TO  FLG-PTMEMOINF
               ELSE
                   MOVE    1                   TO  FLG-PTMEMOINF
                   INITIALIZE                  PTMEMOINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
               MOVE    "1"             TO  SPA-NAI-MEMOKBN
           ELSE
               MOVE    SPACE           TO  SPA-NAI-MEMOKBN
           END-IF
      *
           .
       41021-PTMEMOINF-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  K02N-PTNUM
      *            受付一覧からの時、診療科を編集する
                   IF      WRK-MOTOPG          =   "U01"
                       MOVE    WRK-SPA-SRYKA   TO  SPA-SRYKA
                   END-IF
                   PERFORM 41010-PTNUM-SYORI-SEC
      *!!          ＤＢエラー
                   IF     (FLG-END2        =   1      )
                      OR  (SPA-ERRCD       =   "1999" )
                       MOVE    "E"             TO  SPA-K01-YOBI(1:1)
                   END-IF
      *!!
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *!!
                   IF     (FLG-END2        =   1      )
                       MOVE    90              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-PTID            =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               MOVE    ZERO                TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-K98-SET-SEC          SECTION.
      *
      **** MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-K98 FROM    1   BY  1
                   UNTIL      (IDX-K98             >   10  )  OR
                              (SPA-K98-SRYCD(IDX-K98)  =   SPACE ) OR
                              (SPA-ERRCD           NOT =   SPACE )
      *
               IF      IDX-K98             =   1
                   INITIALIZE                  SPA-COM-TBLREC
                                                       (SPA-GMN-IDX)
                   INITIALIZE                  SPA-GMN-TBLREC
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-K98-SRYCD(IDX-K98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPA-K98-SRYCD(IDX-K98)
                                               TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
               END-IF
      *
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-K98-SET-EXT.
           EXIT.
      *@@
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       5101-AKUSEI-K98-SEC       SECTION.
      *
           MOVE    SPA-AKUSEI-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-K98-AREA
           MOVE    "//*D009S"          TO  SPA-K98-INPUTCD
           MOVE    "A"                 TO  SPA-K98-CDSYUBETU
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-AKUSEI-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-K98-SYORI-SEC
           .
       5101-AKUSEI-K98-EXT.
           EXIT.
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       30112-AKUSEI-COMMENT-SEC       SECTION.
      *
           ADD     1                   TO  SPA-GMN-IDX 
           IF      SPA-GMN-IDX         <=  SPA-GMN-MAX
               PERFORM 41014-GYOINSN-SEC
           END-IF
           MOVE    SPA-GMN-IDX         TO  IDX-S
           IF      SPA-K98-SRYCD-G   NOT =   SPACE
               PERFORM 3011-K98-SET-SEC
           ELSE
      *    コメントコード追加
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
      *
      *H30.9
      *****************************************************************
      *    選択式コメントへ処理
      *****************************************************************
       30113-SELCOMMNT-COMMENT-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-AUTOERR
           MOVE    ZERO                TO  IDX-SELCOM
      *
           IF     (SPA-SELCOM-IDX      >   ZERO  )
             AND  (SPA-COM-INPUTCD(SPA-SELCOM-IDX)(1:1)  =   "1"  )
             AND  (SPA-COM-RECEKISAI (SPA-SELCOM-IDX)    =   "1"
                                                         OR  "2"  )
      *        選択済み
               MOVE    "1"             TO  SPA-COM-RECEAUTO
                                                    (SPA-SELCOM-IDX)
      *!
      *        回数入力の下に追加
               IF     (SPA-COM-KAIFLG (SPA-SELCOM-IDX)  =   "1"  )
                 AND  (SPA-COM-ZAIEND (SPA-SELCOM-IDX)  =   "1"  )
                 AND  (SPA-K98-SRYCD-G     NOT =   SPACE  )
                   MOVE    1                   TO  FLG-AUTOERR
                   MOVE    SPA-SELCOM-IDX      TO  IDX-SELCOM
               END-IF
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
           PERFORM 3011-K98-SET-SEC
      *
      *    コメント挿入行前で剤終了は警告
           IF      FLG-AUTOERR         =   1
               MOVE    "1"              TO  SPA-COM-CUR
                                                    (IDX-SELCOM)
               MOVE    "K117"           TO  SPA-ERRCD
           END-IF
      *
           MOVE    1                   TO  FLG-GMN
           .
       30113-SELCOMMNT-COMMENT-EXT.
           EXIT.
      *
      *@@
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0103"
                   MOVE    SPACE               TO  SPA-KIDCD
      *************IF      SPA-KID1-FLG        =   "OK"
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       MOVE    1               TO  SPA-CONTFLG
      *                Ｋ０１画面へ
                       PERFORM 4001-K01-SYORI-SEC
      *************END-IF
      *
               WHEN    "0104"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                終了処理へ
                       PERFORM 210-BACK-OK-SEC
                   END-IF
      *        保険組合せ変更確認
               WHEN    "0105"
               WHEN    "0124"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    ZERO            TO  FLG-CHK2
                       PERFORM 41012-HKNCOMBI-CHANGE-SEC
                   ELSE
                       MOVE    SPA-MAE-HKNCOMBI-G
                                           TO  SPA-GMN-HKNCOMBI-G
                       PERFORM 3101-HKNCOMBI-HEN-SEC
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0107"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41013-SRYKA-CHANGE-SEC
      *!!
               WHEN    "0138"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41013-SRYKA-CHANGE02-SEC
      *!!
      *        診療科変更確認
               WHEN    "0122"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    3               TO  FLG-PARASET
                       PERFORM                 3002-PARA-SET-SEC
      *                画面用スパ編集処理
                       PERFORM                     310-SPA-GMNSET-SEC
      *                ワーク診療行為クリア
                       MOVE    1                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                   ELSE
      *                ワーク診療行為クリア
                       MOVE    2                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0123"
               WHEN    "0128"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   ELSE
                       MOVE    SPA-MAE-SRYKA-G     TO  SPA-GMN-SRYKA-G
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
      *        診療日が未来日の確認
               WHEN    "0111"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                診療日確定後、編集
                       PERFORM 410171-SRYYMD-OK-SEC
                   ELSE
                       MOVE    7               TO  SPA-GMN-CUR
                   END-IF
      *
      *        特定疾患処方管理加算の算定確認
               WHEN    "0113"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TOKUTEIFLG
                   ELSE
                       MOVE    1                   TO  SPA-TOKUTEIFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *
      *        クリアの確認
               WHEN    "0116"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 220-CLEAR-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        中途終了データのクリア
               WHEN    "0127"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                ワーク診療行為削除
                       PERFORM 2202-CLEAR-WKSRYACT-SEC
                   ELSE
      *                ワーク診療行為解除
                       MOVE    ZERO            TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        保険変更時、中途終了あり
               WHEN    "0118"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                内容置換え
                       PERFORM 410132-SAIHENSYU-SEC
                   ELSE
      *                ワーク診療行為クリア
                       MOVE    2                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                   END-IF
      *        中途終了時刷確認
               WHEN    "0117"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 4510-SNRYO-NYU-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        退院時外来へ
               WHEN    "0120"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                外来へ
                       MOVE    SPA-GMN-PTID        TO  SPA-PTID
                       MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
                       INITIALIZE                  SPA-K01KYOUTU
                       INITIALIZE                  SPA-K02
                       MOVE    "JOIN"              TO  WRK-PUTTYPE
                       PERFORM 410109-K02IDOU-SEC
                   ELSE
      *                退院日を診療日へ
                       IF      SPA-K02N-TAIINYMD   =   SPACE
                                                   OR  "99999999"
                           MOVE    SPA-SYSYMD          TO  WRK-SYMD
                       ELSE
                           MOVE    SPA-K02N-TAIINYMD   TO  WRK-SYMD
                       END-IF
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
                       MOVE    WRK-SYMD            TO  SPA-SRYYMD
                       MOVE    SPA-GMN-PTNUM       TO  K02N-PTNUM
                       MOVE    "1"                 TO  SPA-NYUGAIKBN
                       PERFORM 41010-PTNUM-SYORI-SEC
                   END-IF
      *
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 30016-KENSA-DELETE-SEC
                   END-IF
      *
      *        リハビリ逓減確認
               WHEN    "0180"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                        MOVE    1                  TO  FLG-RIHTEI
      *                 診療行為、計計算処理、編集
                        MOVE    SPACE              TO  FLG-TOROKU
                        PERFORM 510-TBLHEN-SEC
                        MOVE    SPACE              TO  FLG-TOROKU
                   ELSE
                        MOVE    ZERO               TO  FLG-RIHTEI
      *                 診療行為、計計算処理、編集
                        MOVE    SPACE              TO  FLG-TOROKU
                        PERFORM 510-TBLHEN-SEC
                        MOVE    SPACE              TO  FLG-TOROKU
                   END-IF
      *        クリア確認チェック
               WHEN    "0125"
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 2221-PTNUMCLEAR-SYORI-SEC
                   ELSE
                        MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *
      *        アフターケア選択 外来へ
               WHEN    "0126"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                外来へ
                       MOVE    SPA-GMN-PTID        TO  SPA-PTID
                       MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
                       MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
                       INITIALIZE                  SPA-K01KYOUTU
                       INITIALIZE                  SPA-K02
                       MOVE    "JOIN"              TO  WRK-PUTTYPE
                       PERFORM 410109-K02IDOU-SEC
                   ELSE
                       MOVE    SPA-MAE-HKNCOMBI-G
                                               TO  SPA-GMN-HKNCOMBI-G
                       MOVE    5               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *@
      *        包括エラーの削除
               WHEN    "0150"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 51091-HKTSANTEI-DEL-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *ver.4.7
      *        同日再入院の変更確認
               WHEN    "0200"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    1                   TO  FLG-NYUNEXT
                   ELSE
                       MOVE    ZERO                TO  FLG-NYUNEXT
                   END-IF
                   PERFORM 41251-NYUINRRK-NEXTSYORI-SEC
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ２）ＯＫ処理
      *****************************************************************
       30011-KID2-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-KID2-FLG
           MOVE    SPACE               TO  SPA-KIDCD2
           .
       30011-KID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択（Ｋ０９）画面より処理
      *****************************************************************
       3013-K09-SET-SEC                  SECTION.
      *
      *    選択がない時、そのまま
           IF      SPA-K09-SELOK       =   ZERO
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      3013-K09-SET-EXT
           END-IF
      *
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    ＤＯ画面（Ｋ０９）より、ＤＯ内容追加編集
           MOVE    SPA-GMN-MAX         TO  IDX2
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
           MOVE    SPA-GMN-MAX         TO  WRK-STR
      *
      *    再表示編集処理
           IF     (SPA-K09-SELOK       =   1    )  AND
                  (LNK-WKSRYACT-MAX    >   ZERO )
      *********MOVE    ZERO            TO  FLG-SYOSIN
               MOVE    SPACE               TO  WRK-ERRCD1
               MOVE    1               TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
      *
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
               END-IF
      *H28.4   再チェックを行う
               IF      WRK-STR         =   ZERO
                   MOVE    1               TO  WRK-STR
               ELSE
                   ADD     1               TO  WRK-STR
               END-IF
               PERFORM VARYING IDX     FROM    WRK-STR   BY  1
                       UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                              ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                               (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   MOVE    SPACE       TO  SPA-MAE-INPUTCD(IDX)
               END-PERFORM
      *
      *        診療行為、計計算処理、編集
               MOVE    SPACE           TO  FLG-TOROKU
               MOVE    1               TO  FLG-SYOKI
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE           TO  FLG-TOROKU
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3013-K09-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コード設定より編集処理
      *****************************************************************
       30014-K023-SET-SEC                  SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-GMN-INPUTCD (IDX)(1:1)  =   "." OR "S" )  OR
                      (SPA-COM-INPUTCD (IDX)       =   SPACE     ) 
                   CONTINUE
               ELSE
      *
                   MOVE    SPACE               TO  INPUTCD-REC
                   MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
                   MOVE    SPA-COM-INPUTCD(IDX) 
                                               TO  ICD-SRYCD
      *
                   MOVE    "key4"              TO  WRK-PATH
      *
                   PERFORM 930-INPUTCD-READ-SEC
                   MOVE    SPACE           TO  SPA-COM-GMNFLG (IDX)
                   IF      FLG-INPUTCD         =   ZERO
                       MOVE    ICD-INPUTCD     TO  SPA-COM-ICD-INPUTCD
                                                              (IDX)
                   ELSE
                       MOVE    SPACE           TO  SPA-COM-ICD-INPUTCD
                                                              (IDX)
                   END-IF
                   MOVE    SPACE               TO  WRK-PATH
      *
      *            画面再編集・基本チェック
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "2"                 TO  LNK-SC94-KBN
                   MOVE    "K02"               TO  LNK-SC94-PG
                   MOVE    IDX                 TO  LNK-SC94-IDX
                   MOVE    SPA-SYORIFLG        TO  LNK-SC94-SYORIFLG
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               SPA-SCR-REC
                                               SPA-AREA
               END-IF
           END-PERFORM
      *
           MOVE    1                    TO  SPA-GMN-CUR
      *
           .
       30014-K023-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    指示せん発行画面（Ｋ０８１）より処理
      *****************************************************************
       3014-K081-SET-SEC                  SECTION.
      *
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           EVALUATE    SPA-K081-OKFLG
               WHEN    ZERO
                   MOVE    1               TO  SPA-GMN-CUR
               WHEN    1
      *        登録・発行
      *            発行有無判定
                   IF      (SPA-K081GMN-SYOHOU     =   "1" )  OR
                           (SPA-K081GMN-CHUSYA     =   "1" )  OR
                           (SPA-K081GMN-SIJISEN    =   "1" )
                       MOVE    1               TO  FLG-SIJISEN
                   ELSE
                       MOVE    ZERO            TO  FLG-SIJISEN
                   END-IF
      *            中途終了登録
                   PERFORM 4510-SNRYO-NYU-SEC
               WHEN    2
      *        発行のみ
                   IF      (SPA-K081GMN-SYOHOU     =   "1" )  OR
                           (SPA-K081GMN-CHUSYA     =   "1" )  OR
                           (SPA-K081GMN-SIJISEN    =   "1" )
                       MOVE    "2"                 TO  FLG-SYORI
                       PERFORM 45101-SNRYO-SYORI-SEC
      *                入院指示せん発行処理
                       PERFORM 45900-SIJISEN-SYORI-SEC
                   END-IF
           END-EVALUATE
      *
           .
       3014-K081-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一検査削除処理
      *****************************************************************
       30016-KENSA-DELETE-SEC                  SECTION.
      *
      *    診療行為、計計算処理、編集
           MOVE    1                   TO  FLG-KENSADEL
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    ZERO                TO  FLG-KENSADEL
           MOVE    SPACE               TO  FLG-TOROKU
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30016-KENSA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       3002-PARA-SET-SEC                  SECTION.
      *
      *    MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *    MOVE    SPACE               TO  LNK-WKSRYACT-AREA.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    2               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *        UNTIL   FLG-PARA        NOT =   ZERO
      *        IF      PARA-FILEMEI        =   "WKSRYACT"
      *            IF      PARA-DATA-REC   NOT =   SPACE
      *                ADD     1                   TO  LNK-WKSRYACT-MAX
      *                MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
      *                                              (LNK-WKSRYACT-MAX)
      *            END-IF
      *        END-IF
      *
      *        IF      PARA-FILEMEI        =   "K01PARA"
      *            IF      FLG-PARASET     =   ZERO    OR  1
      *                MOVE    PARA-DATA-REC       TO  SPA-K01KYOUTU
      *            END-IF
      *        END-IF
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      ***  CLOSE                       PARA-FILE
      *
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
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
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
                  OR  (SPA-ERRCD       NOT =   SPACE )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "9000"              TO  SPA-ERRCD
               ELSE
                   MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           IF      FLG-PARASET         =   ZERO  OR  1
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    "K02"               TO  SPATMP-GYOUMUID
               MOVE    SPA-TERMID          TO  SPATMP-TERMID
               MOVE    "K01KYOUTU"         TO  SPATMP-FILEMEI
               MOVE    1                   TO  SPATMP-EDANUM
               PERFORM 9100-SPATMP-KEYREAD-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO  SPA-K01KYOUTU
               END-IF
           END-IF
      *
           IF      FLG-PARASET         =   ZERO  OR  2
      *****    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *        MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *        OPEN    INPUT               PARAK02-FILE
      *        IF      STS-PARA2           =  ZERO
      *            READ    PARAK02-FILE
      *                AT  END
      *                    INITIALIZE                  PARA-SPA-K02
      *                NOT AT  END
      *                    MOVE    PARA-SPA-K02        TO  SPA-K02
      *            END-READ
      *        END-IF
      *********CLOSE                           PARAK02-FILE
      *
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    "K02"               TO  SPATMP-GYOUMUID
               MOVE    SPA-TERMID          TO  SPATMP-TERMID
               MOVE    "K02SPA"            TO  SPATMP-FILEMEI
               MOVE    1                   TO  SPATMP-EDANUM
               PERFORM 9100-SPATMP-KEYREAD-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO  SPA-K02
               END-IF
      *
           END-IF
      *
           .
       3002-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *    見出し
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
           MOVE    SPA-FTNRATE         TO  SPA-GMN-FTNRATE
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    保険組合
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           PERFORM 3101-HKNCOMBI-HEN-SEC
           IF     (SPA-KIDCD       NOT =   SPACE )
               GO      TO      310-SPA-SET-EXT
           END-IF
      *
      *    時間外区分
           MOVE    "0 時間内"          TO  SPA-K02N-JIKANKBN-LST(1)
           MOVE    "1 時間外"          TO  SPA-K02N-JIKANKBN-LST(2)
           MOVE    "2 休日"            TO  SPA-K02N-JIKANKBN-LST(3)
           MOVE    "3 深夜"            TO  SPA-K02N-JIKANKBN-LST(4)
           MOVE    "4 時間外特例"      TO  SPA-K02N-JIKANKBN-LST(5)
           IF      SPA-K02N-JIKANKBN   =   SPACE
               MOVE    "0"             TO  SPA-K02N-JIKANKBN
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-K02N-JIKANKBN   =   SPA-K02N-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-K02N-JIKANKBN-LST (IDX)
                                           TO  SPA-K02N-JIKANKBN-G
                   MOVE   5                TO  IDX
               END-IF
           END-PERFORM
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *R02.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    画面表示用
           IF      SPA-NENREI-YY       =   ZERO
               MOVE    SPA-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    SPA-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-NENREI-HEN
      *
      *    診療日の末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYLST-DD
           ELSE
               MOVE    31                  TO  SPA-SRYLST-DD
           END-IF
      *
      *    ＤＯ編集
           MOVE    "F"                 TO  SPA-GMN-TEISEI
           MOVE    ZERO                TO  SPA-NAI-SYORI
      *
      *!!!
      *    入院料包括の判定（Ｈ２０．４から）
           INITIALIZE                  SPA-NYUHKT-AREA
           IF     (SPA-HKTNYUIN-FLG    =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")  AND
                  (SPA-CHIKENKBN   NOT =   "1"   )  AND
                  (SPA-SRYYMD          >=  "20080401" )
               PERFORM 3101-HKTCHK-INIT-SEC
           END-IF
      *!!!
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料包括の判定チェック処理
      *****************************************************************
       3101-HKTCHK-INIT-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    入院料包括の期間チェック
           INITIALIZE                  ORCSCHKTCHKAREA
           INITIALIZE                  ORCSC00AREA
           MOVE    "5"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO   LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    ORCSCHKTCHK-NYUHKTCHK   TO  SPA-NYUHKTCHK
           MOVE    ORCSCHKTCHK-NYUHKTCD    TO  SPA-NYUHKTCD
           MOVE    ORCSCHKTCHK-NYUSRYCD    TO  SPA-NYUHKTSRYCD
           MOVE    ORCSCHKTCHK-NYUHKT-G    TO  SPA-NYUHKT-G
      *    入院料算定なし
           MOVE    ORCSCHKTCHK-NYUERR  TO  SPA-NYU-ERR
      *    翌月入院料算定なし
           MOVE    ORCSCHKTCHK-NYUERR-2    TO  SPA-NYU-ERR-2
           .
       3101-HKTCHK-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合決定編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "H"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           IF      SPA-HKN-ERR     NOT =   4
      *    アフターケアの場合エラーとする
               IF     (SPA-HKNNUM          =   "971" ) AND
                      (SPA-RSI-HKNKBN      =   "3" )
                   IF      SPA-SYORIFLG        =   1
                       MOVE    "9029"              TO  SPA-ERRCD
                   ELSE
      *            外来へ
                       MOVE    "0126"              TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       310-SPA-GMNSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      310-SPA-GMNSET-EXT
           END-IF
      *
      *****MOVE    "2"                 TO  FLG-TOROKU
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    SPACE               TO  WRK-ERRCD1
      *
      *    再表示編集処理
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    ZERO            TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
           END-IF
      *    受診回数編集処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *    初診料自動発生
           IF      (SPA-NYU-SYOSIN     =   "1" )  AND
                   (SPA-SYORIFLG       =   ZERO)
      *        中途終了展開時の自動発生
               IF     (SPA-CHUTOTENKAI-FLG     =   "0" ) AND
                      (SPA-WKSRY-KARTE-FLG     =   1   ) AND
                      (LNK-WKSRYACT-MAX        >   ZERO)
                   CONTINUE
               ELSE
                   PERFORM 330-SYOSIN-SET-SEC
               END-IF
           END-IF
      *
      *    時間外区分
           MOVE    SPA-NAI-TIMEFLG     TO  SPA-K02N-JIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-K02N-JIKANKBN   =   SPA-K02N-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-K02N-JIKANKBN-LST (IDX)
                                           TO  SPA-K02N-JIKANKBN-G
                   MOVE    5               TO  IDX
               END-IF
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           MOVE    ZERO               TO  SPA-NAI-WKSRYERR
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD (IDX)
                                           TO  SPA-MAE-INPUTCD (IDX)
               ELSE
                   MOVE    1               TO  SPA-NAI-WKSRYERR
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD (IDX)
               END-IF
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG(IDX)
               MOVE    "1"                 TO  SPA-COM-HKEIFLG (IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLGNK (IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG2(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG3(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG4(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG5(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG6(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG1(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG(IDX)
      *
               MOVE    "1"                 TO  SPA-COM-KZMFLGN2 (IDX)
           END-PERFORM
      *    内分泌負荷試験検査情報
           INITIALIZE              SPA-NAI-FUKATEN-AREA
      *    診療行為、計計算処理、編集
           MOVE    1                   TO  FLG-INIT
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *    訂正時点数変更チェック
           MOVE    ZERO                TO  SPA-TEISEI-CHK
           MOVE    ZERO                TO  SPA-TEISEI-ERR
      *    警告フラグクリア
           MOVE    ZERO                TO  WRK-MAE-TENSU
           MOVE    ZERO                TO  FLG-MAE-TENSU
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF     (SPA-COM-KZMFLG (IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG2(IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG3(IDX)    NOT =   SPACE )  OR
                      (SPA-COM-NYU-KZMFLG-G(IDX)    NOT =   SPACE )
      *ver.4.7        入院料併用警告
                 OR   (SPA-COM-KZMFLGNK(IDX)   NOT =   SPACE )
      *H29.1        入院料併用警告
                 OR   (SPA-COM-KZMFLGN2(IDX)   NOT =   SPACE )
      *            約束セット
                   IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "." )
      *?              OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                      CONTINUE
                   ELSE
                       MOVE    SPACE       TO  SPA-MAE-INPUTCD (IDX)
                   END-IF
               END-IF
      *        警告フラグクリア
               MOVE    SPACE           TO  SPA-COM-KZMFLGAREA (IDX)
               MOVE    SPACE           TO  SPA-COM-NYU-KZMFLG-G(IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG (IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1(IDX)
      *        併用算定が警告の時、クリア
               IF      SPA-COM-HKEIFLG (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-COM-HKEIFLG (IDX)
                   MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
               END-IF
      *TEST
      *H30.9
      *        訂正展開時、選択コメント対応済みとする
               IF     (SPA-SYORIFLG        =   1  )
                 AND  (FLG-TEISEI          =   1  )
                   IF     (SPA-COM-INPUTCD (IDX) (1:1) =   "1" )
                      AND (SPA-COM-RECEKISAI(IDX) NOT  =   SPACE)
                       MOVE    "1"         TO  SPA-COM-RECEAUTO
                                                            (IDX)
                   END-IF
               END-IF
      *
      *        訂正時の点数判定
               IF      SPA-SYORIFLG        =   1
                   IF      FLG-MAE-TENSU   =   ZERO
                       IF     (SPA-COM-MAE-TENSU(IDX)  >   ZERO  )  OR
                              (SPA-COM-INPUTCD (IDX)   NOT =   SPACE)
                           IF     (SPA-COM-MAE-TENSU(IDX)  =   ZERO) AND
                                  (SPA-COM-INPUTCD (IDX)(1:1)  =  "8"
                                                               OR "0")
                               CONTINUE
                           ELSE
                               MOVE    SPA-COM-MAE-TENSU(IDX)
                                                   TO  WRK-MAE-TENSU
                               MOVE    1           TO  FLG-MAE-TENSU
                           END-IF
                       END-IF
                   END-IF
                   IF     (SPA-COM-ZAIEND(IDX)     =   "1" )  OR
                         ((SPA-COM-SRYKBN(IDX)     =   "95"
                                                   OR  "96" ) AND
                          (SPA-COM-JIHIMONEY(IDX)  >   ZERO ))
                       IF      SPA-COM-SRYKBN(IDX)     =   "95"
                                                       OR  "96"
                           MOVE    SPA-COM-JIHIMONEY(IDX)
                                                   TO  WRK-TENSU
                       ELSE
                           MOVE    SPA-COM-TENSU (IDX)
                                                   TO  WRK-TENSU
                       END-IF
                       IF      WRK-TENSU       NOT =   WRK-MAE-TENSU
                           MOVE    1               TO  SPA-TEISEI-ERR
                           MOVE    "1"             TO  SPA-COM-CUR (IDX)
                       END-IF
                       MOVE    ZERO            TO  WRK-MAE-TENSU
                       MOVE    ZERO            TO  FLG-MAE-TENSU
                   END-IF
                   IF      SPA-COM-ZAIEND(IDX)     =   "1"
                       MOVE    ZERO                TO  WRK-MAE-TENSU
                       MOVE    ZERO                TO  FLG-MAE-TENSU
                   END-IF
               END-IF
           END-PERFORM
      *
      *    画面ＳＰＡ書込み（画面遷移前）
           IF      FLG-END             =   1
               PERFORM 1003-K02SPA-OUT-SEC
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *    内分泌負荷試験検査情報
           INITIALIZE              SPA-NAI-FUKATEN-AREA
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-ERRCD           =   SPACE)
               MOVE    "K999"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-TEISEI-CHK
           END-IF
      *
           .
       310-SPA-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
      *    処理区分（０：訂正展開、１：中途終了展開、２：ＤＯ展開）
           IF      FLG-K09TUIKA        =   1
               MOVE    2               TO  ORCSCWKSRY-SYORIFLG
           ELSE
               MOVE    1               TO  ORCSCWKSRY-SYORIFLG
               IF      SPA-SYORIFLG    =   1
                   MOVE    ZERO            TO  ORCSCWKSRY-SYORIFLG
               END-IF
           END-IF
      *    中途終了分追加フラグ
           IF      FLG-K09TUIKA        =   3
              MOVE    1                TO  ORCSCWKSRY-K09TUIKA
           END-IF
      *    追加開始位置
           MOVE    IDX2                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  WRK-ERRCD1
           END-IF
      *
      *H24.9
      *    中途終了でアフターケアの場合エラーとする
           IF     ORCSCWKSRY-SYORIFLG  =   1
               IF     (SPA-HKNNUM          =   "971" ) AND
                      (SPA-RSI-HKNKBN      =   "3"   )
                   MOVE    "9041"              TO  SPA-ERRCD
                   MOVE    "9041"              TO  WRK-ERRCD1
                   MOVE    9                   TO  SPA-HKN-ERR
                   MOVE    SPACE               TO  SPA-ERRMSG2
               END-IF
           END-IF
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       320-SPA-CHKSET-SEC              SECTION.
      *
      *    当月診療行為チェック用編集、当月累計点数計算
           MOVE    ZERO                TO  SPA-ZAITENTOTAL
           MOVE    SPACE               TO  SPA-ACCT2-SRYCD
           MOVE    ZERO                TO  SPA-ACCT2-ZAINUM
           MOVE    ZERO                TO  SPA-ACCT2-CSYYOURYO
           MOVE    SPACE               TO  SPA-CHOKI-ARI
      *****PERFORM 3201-TOUCHKSET-SEC
      *
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  =   ZERO
               INITIALIZE                  SPA-KNKYAK-AREA
           ELSE
      *        禁忌薬剤チェック用編集
               PERFORM 3202-KNKYAKSET-SEC
           END-IF
      *
           .
       320-SPA-CHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3202-KNKYAKSET-SEC              SECTION.
      *
           INITIALIZE                  ORCSCKNKCHKAREA
           MOVE    "1"                 TO  ORCSCKNKCHK-KBN 
           CALL    "ORCSCKNKCHK"       USING
                                       ORCSCKNKCHKAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           .
       3202-KNKYAKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       330-SYOSIN-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK3
      *    初診料が算定できる時
           IF       SPA-SYOSIN         =   1
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-OK3
           END-IF
      *
      *    入院年月であること
           IF      SPA-K02N-NYUINYMD(1:6)  NOT =   SPA-SRYYMD(1:6)
               MOVE    1                   TO  FLG-OK3
           END-IF
      *
      *    初診料がないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )
                           OR (SPA-GMN-INPUTCD (IDX)   =   SPACE )
                           OR (FLG-OK3     =   1        )
               IF      SPA-COM-SRYKBN (IDX)    =   "11"
                   MOVE    1               TO  FLG-OK3
               END-IF
           END-PERFORM
           IF      FLG-OK3             =   1
               MOVE    ZERO                TO  IDX2
           ELSE
      *        初診料自動発生
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "1"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    LNK-SC10S-IDX2      TO  IDX2
           END-IF
      *
      *    入院日であること
           IF     (SPA-K02N-NYUINYMD   NOT =   SPA-SRYYMD )  AND
                  (IDX2                >   ZERO           )
               ADD     1                   TO  IDX2
               MOVE    IDX2                TO  SPA-GMN-IDX
      *        行挿入処理
               PERFORM 41014-GYOINSN-SEC
               MOVE    SPA-K02N-NYUINYMD(7:2)  TO  WRK-SDD
               MOVE    "*/"                TO  SPA-GMN-INPUTCD (IDX2)
               MOVE    WRK-SDD             TO  SPA-GMN-INPUTCD (IDX2)
                                                               (3:2)
           END-IF
      *
           .
       330-SYOSIN-SET-EXT.
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
      *        Ｋ０１へ
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 4001-K01-SYORI-SEC
      *        患者取消
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 222-PTNUMCLEAR-SEC
      *        途中終了
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 4110-KYOSEIEND-SEC
      *        途中終了画面へ（Ｋ１０）
               WHEN    "CLICKED"       ALSO    "B12CS"
                   PERFORM 4110S-K10-SYORI-SEC
      *        セット登録（Ｋ０５）
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 420-KOUISET-SEC
      *        前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *        算定履歴更新（Ｋ０６）
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 430-SANTEIRRK-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 220-CLEAR-SEC
      *        患者登録へ（Ｐ０２）
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 440-KNJADD-SEC
      *        病名登録　（Ｃ０２）
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 440-BYOTOROKU-SEC
      *        会計カード（Ｊ０２）
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 450-KAIKEI-SEC
      *        収納登録（Ｓ０２）
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 460-SYUNOU-SEC
      *D       前カーソル
      *D       WHEN    "CLICKED"       ALSO    "B06S"
      *D           PERFORM 450-MAECUR-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *        ＤＯ（Ｋ０９）
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-DOSYORI-SEC
      *****    算定（Ｋ０４）
      *        WHEN    "CLICKED"       ALSO    "B23"
      ********     PERFORM 470-SANTEI-SEC
      *        訂正処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 4101-TEISEI-SEC
      *        訂正処理（トグルボタン）
               WHEN    "CLICKED"       ALSO    "TEISEI"
                   PERFORM 4101-TEISEI-SEC
      *        氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE               TO  WRK-P97-NAME
                   PERFORM 470-P97-SIMEIKEN-SEC
      *        予約登録へ（Ｙ０１）
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *        受付一覧へ（Ｕ０１）
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *        受付画面へ（Ｕ０２）
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 4102-UKETUKE-SEC
      **       登録（Ｋ０８）
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    SPACE           TO  WRK-PUTTYPE
                   PERFORM 4100-TOROKU-SEC
      *        入力コード設定へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 4106S-INPUTCD-SYORI-SEC
      *        入退院登録へ
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 4106S-NYUTAIIN-SEC
      *        入院会計照会へ
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4111S-NYUKAIKEI-SEC
      *        ヘルプ
               WHEN    "CLICKED"       ALSO    "BHELP"
                   PERFORM 4199-HELP-SYORI-SEC
      *        検査処理（トグルボタン）
               WHEN    "CLICKED"       ALSO    "KENSA"
                   PERFORM 4109-KENSA-SEC
      *        受診履歴 前頁
               WHEN    "CLICKED"       ALSO    "BC06"
                   PERFORM 4109-BC06-SYORI-SEC
      *        受診履歴 次頁
               WHEN    "CLICKED"       ALSO    "BC07"
                   PERFORM 4109-BC07-SYORI-SEC
      *        投薬帳票印刷
               WHEN    "CLICKED"       ALSO    "BSH1"
                   PERFORM 4124-MAESYOHOU-SYORI-SEC
      *        同日再入院日へ
               WHEN    "CLICKED"       ALSO    "BNYU"
                   PERFORM 4125-NYUINRRK-NEXT-SEC
      *H26.4
      *        １行目へ
               WHEN    "CLICKED"       ALSO    "CTRLHOME"
                   PERFORM 4501-CTRHOME-SEC
      *H29.5
      *        メモ登録へ
               WHEN    "CLICKED"       ALSO    "BSH2"
                   PERFORM 4188-MEMO-SYORI-SEC
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
      *    入力項目の決定
           IF      MCP-EVENT           =   "ACTIVATE"   OR
                                           "ACTIVATE2"
      *ver.4.7 IF     (MCP-WIDGET(1:07)    =   "INPUTCD"  )  OR
      ******          (MCP-WIDGET(1:6)     =   "MEISYO"   )
               IF      MCP-WIDGET      =   "PANDATABLE1"
                   PERFORM 2001-INPUT-SET-SEC
                   IF      SPA-ERRCD   NOT =   SPACE
                       GO      TO      200-ENTRY-EXT
                   END-IF 
                   IF      WRK-IDX2        =   ZERO
                       GO      TO      200-ENTRY-EXT
                   END-IF 
               ELSE
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
               END-IF
           ELSE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
           END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    WRK-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        保険番号入力
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 41012-HKNCOMBI-SYORI-SEC
      *        診療日入力
               WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                   PERFORM 41017-SRYYMD-SYORI-SEC
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        時間外入力
               WHEN    "ACTIVATE"      ALSO    "JIKANKBN"
                   PERFORM 41019-JIKANKBN-SYORI-SEC
      *        入力コード
               WHEN    "ACTIVATE"      ALSO    "INPUTCD"
                   PERFORM 41011-KOUI-ALLHEN-SEC
      *        入力コード変換
      ******** WHEN    "ACTIVATE2"     ALSO    "INPUTCD"
      *******      PERFORM 41014-INPUTCD-MODHEN-SEC
      *        名称
               WHEN    "ACTIVATE"      ALSO    "MEISYO"
                    PERFORM 41011-KOUI-ALLHEN-SEC
      *        ＤＯ選択
               WHEN    "ACTIVATE"      ALSO    "DOSELNUM"
                   PERFORM 41014-DOSELNUM-SEC
      *        診療日選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41015-RRKLIST-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    SPACE               TO  FLG-TOROKU
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
      **** IF      MCP-WIDGET(1:07)    =   "INPUTCD"
      *        MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
      *        MOVE    MCP-WIDGET(1:7)     TO  WRK-WIDGET
      *    END-IF
      *    IF      MCP-WIDGET(1:6)     =   "MEISYO"
      *        MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
      *        MOVE    MCP-WIDGET(1:6)     TO  WRK-WIDGET
      **** END-IF
      *    入力位置の判定
           IF      MCP-WIDGET          =   "PANDATABLE1"
               MOVE    K02N-PANDATABLE1-TROW   TO  WRK-IDX2
               IF      K02N-PANDATABLE1-TCOLUMN    =   2
                   MOVE    "INPUTCD"       TO  WRK-WIDGET
                   MOVE    K02N-PANDATABLE1-TVALUE
                                           TO  K02N-INPUTCD (WRK-IDX2)
               ELSE
                   MOVE    "MEISYO"        TO  WRK-WIDGET
                   MOVE    K02N-PANDATABLE1-TVALUE
                                           TO  K02N-MEISYO  (WRK-IDX2)
               END-IF
           END-IF
      *
      *
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE         TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
      *
               IF      SPA-GMN-IDX         >   SPA-MAX-LINE
                   MOVE    "7001"          TO  SPA-ERRCD
                   MOVE    SPA-MAX-LINE        TO  SPA-GMN-IDX
               END-IF
           END-IF
      *
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
      *
           MOVE    ZERO                TO  WRK-GMN-PTID
           IF      SPA-PTID        NOT =   ZERO
      *        ワーク診療行為クリア
               PERFORM 4900-WKSRYACT-CLEAR-SEC
               MOVE    SPA-PTID        TO  WRK-GMN-PTID
           END-IF
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-GMN-INIT
      *    ＳＰＡクリア処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-K01SET-AREA
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-CONTFLG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPACE               TO  SPA-SRYYMDW(10:)
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *    受付一覧からの時、ドクターを編集する
           IF      WRK-MOTOPG          =   "U01"
               MOVE    WRK-SPA-DRCD    TO  SPA-DRCD
           END-IF
      *
           MOVE    K02N-PTNUM          TO  SPA-GMN-PTNUM
      *
           EVALUATE    TRUE
               WHEN   (SPA-GMN-PTNUM(1:1)  =   "G" OR "g")  AND
                      (SPA-GMN-PTNUM(2:)   =   SPACE     )
      *        ’Ｇ’で外来へ移動する
                   INITIALIZE                  SPA-KYOTUKEY
                   MOVE    SPACE               TO  WRK-PUTTYPE
                   PERFORM 410109-K02IDOU-SEC
      *!!
               WHEN   (SPA-GMN-PTNUM(1:1)  =   "@"   )  AND
                      (SPA-GMN-PTNUM(2:2)  NUMERIC   )  AND
                      (SPA-GMN-PTNUM(4:)   =   SPACE )  AND
                      (SPA-GRPHOSPKBN      =   1     )
      *        @のとき、本院分院切替へ
                   PERFORM 409-GRPCHK-SEC
      *!!
               WHEN   (SPA-GMN-PTNUM           =   SPACE)
                   MOVE    WRK-GMN-PTID        TO  SPA-GMN-PTID
      *        空白は患者取消と同様
                   PERFORM 2221-PTNUMCLEAR-SYORI-SEC
               WHEN    OTHER
      *        患者決定処理
                   PERFORM 410101-PTNUM-IN-SYORI-SEC
           END-EVALUATE
      *
           .
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       410101-PTNUM-IN-SYORI-SEC         SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    10
               WHEN    99
      *            患者番号なし
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    "1003"              TO  SPA-ERRCD
               WHEN    20
      *            氏名検索へ
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    SPTNUM-PTNUM        TO  WRK-P97-NAME
                   PERFORM 470-P97-SIMEIKEN-SEC
               WHEN    OTHER
      *            患者番号決定
                   MOVE    SPTNUM-PTID         TO  SPA-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-PTNUM
                   MOVE    SPA-PTID            TO  SPA-GMN-PTID
                   MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD           =   SPACE
      *                患者番号決定初期処理
                       PERFORM 410101-PTNUM-SYOKI-SEC
      *H29.5
      *            メモ登録有無
                   PERFORM 41021-PTMEMOINF-SEC
      *
      *            患者情報通知
                   PERFORM 41023-POPUP-SYORI-SEC
      *
                   END-IF
           END-EVALUATE
           .
      *
       410101-PTNUM-IN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者番号決定初期処理
      *****************************************************************
       410101-PTNUM-SYOKI-SEC         SECTION.
      *
      *    入院が存在するかのチェック
           PERFORM 410101-NYUINRRK-CHK-SEC
      *    入外区分
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、外来処理へ
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               INITIALIZE                  SPA-K01KYOUTU
               INITIALIZE                  SPA-K02
               MOVE    SPACE               TO  WRK-PUTTYPE
               PERFORM 410109-K02IDOU-SEC
               GO      TO      410101-PTNUM-SYOKI-EXT
           END-IF
           IF     (SPA-KIDCD       NOT =   SPACE ) OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      410101-PTNUM-SYOKI-EXT
           END-IF
      *    受付一覧からの時、診療科を編集する
           IF      WRK-MOTOPG          =   "U01"
               MOVE    WRK-SPA-SRYKA   TO  SPA-SRYKA
               MOVE    WRK-SPA-SRYKA   TO  SPA-GMN-SRYKA
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  WRK-SPA-K01KYOUTU
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    入退院日
      *    MOVE    WRK-NYUINYMD        TO  SPA-K02N-NYUINYMD
      *    MOVE    WRK-TAIINYMD        TO  SPA-K02N-TAIINYMD
           MOVE    WRK-SPA-K02N-DATA-REC   TO  SPA-K02N-DATA-REC
      *
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *    入退院日
      *    MOVE    WRK-NYUINYMD        TO  SPA-K02N-NYUINYMD
      *    MOVE    WRK-TAIINYMD        TO  SPA-K02N-TAIINYMD
      *    MOVE    WRK-SPA-K02N-DATA-REC   TO  SPA-K02N-DATA-REC
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
      *
      *    チェック用月内診療行為セット
           IF      SPA-ERRCD           =   SPACE
               PERFORM 320-SPA-CHKSET-SEC
           END-IF
      *    保険番号が未確定の時、エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-HKNCOMBINUM     =   SPACE )  OR
                      (SPA-GMN-HKNCOMBINUM =   SPACE )
                   MOVE    "1006"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-HKN-ERR
               END-IF
           END-IF
      *
      *    パラメタファイルより表示
           IF      SPA-ERRCD           =   SPACE
               MOVE    3               TO  FLG-PARASET
               PERFORM                 3002-PARA-SET-SEC
      *        画面用スパ編集処理
               MOVE    ZERO            TO  FLG-TEISEI
               PERFORM                 310-SPA-GMNSET-SEC
      *
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    1               TO  SPA-MAP-IDX
           END-IF
           .
       410101-PTNUM-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院存在チェック処理
      *****************************************************************
       410101-NYUINRRK-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSCNYUHENAREA
           MOVE    "1"                 TO  ORCSCNYUHEN-KBN
           MOVE    SPA-SRYYMD          TO  ORCSCNYUHEN-SRYYMD
      *ver.4.7 同日再入院分
           IF      FLG-DOUNYUIN        =   1
               MOVE   "1"                  TO  ORCSCNYUHEN-KBN2
           END-IF
           CALL    "ORCSCNYUHEN"       USING
                                       SPA-AREA
                                       ORCSCNYUHENAREA
      *    入院履歴なし
           IF      ORCSCNYUHEN-RRK-NYUIN   =  ZERO
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           ELSE
               MOVE    "1"                 TO  SPA-NYUGAIKBN
      *        入院中でなく、退院日が当日後
               IF      ORCSCNYUHEN-NYUIN-KBN   =   ZERO
                   IF      ORCSCNYUHEN-LAST-TAIINYMD
                                               >   SPA-SRYYMD
                       MOVE    "2"                 TO  SPA-NYUGAIKBN
                   END-IF
      *            他院履歴のみ（退院日なし）
                   IF     (ORCSCNYUHEN-LAST-TAIINYMD
                                               =   SPACE  )
                       MOVE    "2"                 TO  SPA-NYUGAIKBN
                   END-IF
               END-IF
           END-IF
           IF      SPA-NYUGAIKBN       =   "1"
               IF      ORCSCNYUHEN-NYUIN-KBN   =   ZERO
      *            入院中でなく、退院日がある時
                   MOVE    ORCSCNYUHEN-MAE-TAIINYMD
                                               TO  SPA-K02N-TAIINYMD
                   MOVE    "0120"              TO  SPA-KIDCD
                   MOVE    07                  TO  SPA-GMN-CUR
               ELSE
                   PERFORM 4101011-PTNYUINRRK-HEN-SEC
      *            入院期間
                   MOVE    ORCSCNYUHEN-NYUIN-DAYG
                                              TO  SPA-K02-NYUIN-AREA
               END-IF
      *ver.4.7 同日再入院有無
               MOVE    ORCSCNYUHEN-DOUNYUIN   TO  SPA-K02N-DOUDAY
      *        診療日付＝同日再入院日
               IF     (SPA-SRYYMD             =   SPA-K02N-NYUINYMD)
                 AND  (ORCSCNYUHEN-DOUNYUIN   =   SPA-SRYYMD(7:2)  )
                   MOVE    "1"                TO  SPA-K02N-DOUKBN
               ELSE
                   MOVE    SPACE              TO  SPA-K02N-DOUKBN
               END-IF
      *        同日再入院日へ変更可
               MOVE    ORCSCNYUHEN-DOUNYUKBN  TO  SPA-K02N-DOUNYUKBN
      *        訂正は変更付加
               IF      SPA-SYORIFLG        =   1
                   MOVE    ZERO                TO  SPA-K02N-DOUNYUKBN
               END-IF
               MOVE    ORCSCNYUHEN-DOUTAIKBN  TO  SPA-K02N-DOUTAIKBN
           END-IF
      *    入退院日
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-NYUINYMD
           MOVE    SPA-K02N-TAIINYMD   TO  WRK-TAIINYMD
      *
           .
       410101-NYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴ＳＰＡ編集処理
      *****************************************************************
       4101011-PTNYUINRRK-HEN-SEC      SECTION.
      *
      *    入院科
           MOVE    ORCSCNYUHEN-NYUINKA TO  SPA-K02N-NYUINKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-SRYKA-MAX
               IF      SPA-K02N-NYUINKA    =   SPA-GMN-SRYKAL   (IDX)
                   MOVE    SPA-GMN-SRYKAMEIL(IDX)
                                               TO  SPA-K02N-NYUINKAMEI
                   MOVE    SPA-SRYKA-MAX       TO  IDX
               END-IF
           END-PERFORM
           MOVE    ORCSCNYUHEN-NYUINKA TO  SPA-SRYKA
      *    保険組合せ番号
           MOVE    ORCSCNYUHEN-HKNCOMBI
                                       TO  SPA-HKNCOMBINUM
      *    入院日
           MOVE    ORCSCNYUHEN-NYUINYMD
                                       TO  SPA-K02N-NYUINYMD
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K02N-NYUINYMDW
      *    退院日
           MOVE    ORCSCNYUHEN-TAIINYMD
                                       TO  SPA-K02N-TAIINYMD
           MOVE    "-"                 TO  SPA-K02N-H1
           MOVE    SPA-K02N-TAIINYMD   TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K02N-TAIINYMDW
      *    担当医１（主治医）
           MOVE    ORCSCNYUHEN-DRCD    TO  SPA-DRCD
      *
      *    継続開始日入院日
           MOVE    ORCSCNYUHEN-STR-NYUINYMD
                                       TO  SPA-STR-NYUINYMD
           .
       4101011-PTNYUINRRK-HEN-EXT.
           EXIT.
      *
      *ver.4.8
      *****************************************************************
      *    患者情報通知処理
      *****************************************************************
       41023-POPUP-SYORI-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-POPUP-STATUS
      *
           IF     (SPA-POPUP-HKNCHK    =   "1" )
             OR   (SPA-POPUP-MEMO1     =   "1" )
             OR   (SPA-POPUP-MEMO2     =   "1" )
               INITIALIZE                  ORCSPOPUPSETAREA
      *        保険最終確認日表示
               MOVE    SPA-POPUP-HKNCHK    TO  ORCSPOPUPSET-HKNCHK
      *        メモ１
               MOVE    SPA-POPUP-MEMO1     TO  ORCSPOPUPSET-MEMO1
      *        メモ２
               MOVE    SPA-POPUP-MEMO2     TO  ORCSPOPUPSET-MEMO2
               CALL    "ORCSPOPUPSET"      USING
                                           SPA-AREA
                                           ORCSPOPUPSETAREA
      *
               IF     ORCSPOPUPSET-BODY    NOT =   SPACE
                   MOVE    ORCSPOPUPSET-BODY   TO  SPA-GMN-POPUP-BODY
                   MOVE    "患者情報"          TO  SPA-GMN-POPUP-SUMMARY
                   MOVE    SPACE               TO  SPA-GMN-POPUP-ICON
                   MOVE    SPA-POPUP-TIMEOUT   TO  SPA-GMN-POPUP-TIMEOUT
               END-IF
           END-IF
      *
           .
       41023-POPUP-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来診療行為（Ｋ０２）遷移処理
      *****************************************************************
       410109-K02IDOU-SEC         SECTION.
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      WRK-PUTTYPE     NOT =   SPACE
               MOVE    WRK-PUTTYPE         TO  MCP-PUTTYPE
           ELSE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           END-IF
           MOVE    "K02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410109-K02IDOU-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *    本院分院切替へ
      *****************************************************************
       409-GRPCHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-PTNUM(2:2)  TO  WRK-HOSPNUM
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-GRPHOSPUSERID
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )
                          OR  (FLG-OK  =   1 )
                          OR  (SPA-GRPHOSPNUM (IDX)    =   ZERO )
               IF      WRK-HOSPNUM     =   SPA-GRPHOSPNUM (IDX)
                   MOVE    SPA-GRPHOSPUSERID (IDX)
                                               TO  WRK-GRPHOSPUSERID
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      WRK-GRPHOSPUSERID   =   SPACE
               MOVE    "9210"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-MACHIPG         TO  WRK-MACHIPG
               INITIALIZE                      SPA-AREA
               MOVE    WRK-GRPHOSPUSERID   TO  SPA-OPID
               INITIALIZE                  ORCSCOMMONAREA
               INITIALIZE                  SYS-1010-REC
               MOVE    "K02"               TO  ORCSCOMMON-PG
               MOVE    WRK-HOSPNUM         TO  ORCSCOMMON-HOSPNUM
               CALL    "ORCSCOMMON"        USING
                                           MCPAREA
                                           SPA-AREA
                                           ORCSCOMMONAREA
                                           SYS-1010-REC
               IF      SPA-BEDFLG          =   ZERO
                   MOVE    "M01"               TO  SPA-MACHIPG
               ELSE
                   MOVE    "M01N"              TO  SPA-MACHIPG
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        外来画面へ
               IF      (SPA-BEDFLG         =   ZERO )
                   INITIALIZE                  SPA-KYOTUKEY
                   MOVE    SPACE               TO  WRK-PUTTYPE
                   PERFORM 410109-K02IDOU-SEC
               END-IF
      *        業務選択画面より
               IF      FLG-END             =   ZERO
                   PERFORM 30010-INIT-GMN-SEC
               END-IF
           END-IF
      *
           .
       409-GRPCHK-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *    診療日入力処理
      *****************************************************************
       41017-SRYYMD-SYORI-SEC         SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
      *    訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02N-SRYYMD     NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
               END-IF
               GO      TO      41017-SRYYMD-SYORI-EXT
           END-IF
      *
           MOVE    K02N-SRYYMD         TO  SPA-GMN-SRYYMD
      *    空白の時、システム日付
           IF      SPA-GMN-SRYYMD      =   SPACE
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SRYYMD
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
      *        患者の確定してない時
               IF      SPA-PTID            =   ZERO
                    MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                    MOVE    ZERO                TO  SPA-GMN-CUR
                    GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY          <   SPA-BIRTHDAY
                    MOVE    "1011"         TO  SPA-ERRCD
               END-IF
      *        未来日は確認メッセージ
               IF     (SGDAY-SDAY          >   SPA-SYSYMD    )
                 AND  (SPA-ERRCD           =   SPACE         )
                   MOVE    "0111"              TO  SPA-KIDCD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                   MOVE    ZERO                TO  SPA-HKN-ERR
                   MOVE    ZERO                TO  SPA-RJN-ERR
                   MOVE    ZERO                TO  SPA-NYU-ERR
                   MOVE    ZERO                TO  SPA-NYU-ERR-2
                   MOVE    ZERO                TO  SPA-NAI-KOHFLG-ERR
                   MOVE    ZERO                TO  SPA-NAI-KOHFLG
                   MOVE    ZERO                TO  SPA-NAI-ZENKIFLG-ERR
                   MOVE    ZERO                TO  SPA-NAI-ZENKIFLG
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF     (SGDAY-SDAY          =   SPA-NAI-SRYYMD)
                 AND  (SPA-ERRCD           =   SPACE         )
                 AND  (SPA-KIDCD           =   SPACE         )
      *!
                 AND  (FLG-DOUNYUIN  =   ZERO )
      *!!
                    MOVE    1                  TO  SPA-GMN-CUR
                    GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
               MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
           ELSE
               MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
      *    診療日確定後、編集
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )
               PERFORM 410171-SRYYMD-OK-SEC
           END-IF
      *
           .
       41017-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日確定後、編集処理
      *****************************************************************
       410171-SRYYMD-OK-SEC         SECTION.
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    入院が存在するかのチェック
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           PERFORM 410101-NYUINRRK-CHK-SEC
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    "1"             TO  SPA-NYUGAIKBN
               MOVE    "0120"          TO  SPA-KIDCD
               MOVE    07              TO  SPA-GMN-CUR
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    初診料自動発生
           PERFORM 310-SPA-GMNSET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           .
       410171-SRYYMD-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号入力処理
      *****************************************************************
       41012-HKNCOMBI-SYORI-SEC         SECTION.
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
      *    保険番号入力
           IF    ( K02N-HKNCOMBI       =   SPA-GMN-HKNCOMBI-G)  OR
                 ( K02N-PTNUM          =   SPACE             )
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41012-HKNCOMBI-SYORI-EXT
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
      *        訂正で、継続確定の時、変更不可
               IF      (SPA-CONTKBN        =   "1"   OR  "2")  AND
                       (SPA-DENPJTIKBN     =   "1"   )
                   MOVE    "1013"              TO  SPA-ERRCD
                   GO      TO      41012-HKNCOMBI-SYORI-EXT
               END-IF
      *
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
               MOVE    K02N-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
      *        包括まとめの変更不可
               IF      (SPA-GMN-HKNCOMBINUM     =   "9999" )  OR
                       (SPA-MAE-HKNCOMBINUM     =   "9999" )
                   MOVE    "1033"              TO  SPA-ERRCD
                   MOVE    SPA-MAE-HKNCOMBI-G  TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-MAE-HKNKBN      TO  SPA-HKNKBN
               END-IF
               MOVE    1                   TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
      *        訂正で既に同じ保険の受診履歴がある時、エラー
               IF     (SPA-ERRCD           =   SPACE )
                   PERFORM 410123-JYURRK-HKNCHK-SEC
                   IF      FLG-RRK-ARI     =   1
                       MOVE    "0124"              TO  SPA-KIDCD
                   END-IF
               END-IF
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-KIDCD           =   SPACE )
      *            訂正の時、確認メッセージ表示
                   MOVE    "0105"              TO  SPA-KIDCD
               END-IF
           ELSE
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    K02N-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
               MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
               MOVE    ZERO                TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
           END-IF
           .
       41012-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号変更入力処理
      *****************************************************************
       41012-HKNCOMBI-CHANGE-SEC         SECTION.
      *
           MOVE    SPA-NAI-ROUJIN      TO  WRK-ROUJIN
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           IF     (SPA-ERRCD           NOT =   SPACE )
               OR (SPA-KIDCD           NOT =   SPACE )
               GO      TO      41012-HKNCOMBI-CHANGE-EXT
           END-IF
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           IF     (SPA-NAI-ROUJIN  NOT =   WRK-ROUJIN )  OR
                  (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN)
               PERFORM 410121-ROUJINCHK-SEC
           END-IF
      *
           IF     (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN ) OR
                  (SPA-HKNKBN      NOT =   ZERO           )
      *        リハビリ情報クリア
               INITIALIZE              SPA-RIGAKU-NCNT-AREA
           END-IF
      *
      *    保険区分が変更された時、ワーク診療行為を判定する
           PERFORM 410121-HKNCOMBI-SAI-SEC
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41012-HKNCOMBI-CHANGE-EXT.
           EXIT.
      *****************************************************************
      *    訂正で既に同じ保険の受診履歴判定処理
      *****************************************************************
       410123-JYURRK-HKNCHK-SEC         SECTION.
      *
           MOVE    ZERO                 TO  FLG-RRK-ARI
           PERFORM VARYING     IDX-J    FROM    1   BY  1
                   UNTIL       IDX-J    >   SPA-GMN-MAX1
               IF      (SPA-SRYYMD      =   SPA-NAI-RRKYMD (IDX-J))
                  AND  (SPA-GMN-SRYKA   =   SPA-NAI-RRK-SRYKA(IDX-J))
                  AND  (SPA-GMN-HKNCOMBINUM
                                        =   SPA-NAI-RRK-HKNCOMBI(IDX-J))
                  AND  (SPA-NAI-DOSELNUM    NOT =   IDX-J     )  
                   MOVE    1                TO  FLG-RRK-ARI
               END-IF
           END-PERFORM
           .
       410123-JYURRK-HKNCHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックフラグクリア処理
      *****************************************************************
       410121-ROUJINCHK-SEC         SECTION.
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE               TO  SPA-COM-CHKFLG (IDX)
               MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
           END-PERFORM
           .
       410121-ROUJINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険区分変換処理
      *****************************************************************
       410121-HKNCOMBI-SAI-SEC         SECTION.
      *
      *    受診回数編集処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    SPA-WKSRY-HKNCOMBI  TO  WRK-WKSRY-HKNCOMBI
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           MOVE    "7"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    ワーク診療行為に存在あり
           IF      SPA-WKSRY-HKNCOMBI  NOT =   ZERO
               MOVE    "0118"          TO  SPA-KIDCD
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
           MOVE    WRK-WKSRY-HKNCOMBI  TO  SPA-WKSRY-HKNCOMBI
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-HKNKBN          =   SPA-MAE-HKNKBN  )  AND
                  (SPA-HKNKBN          =   ZERO            )
             AND  (SPA-MAE-HKNCOMBINUM NOT =   "9999"  )
             AND  (SPA-NAI-ROUJIN      =   WRK-ROUJIN  )
               CONTINUE
           ELSE
      *        診療行為、計計算処理、編集
      ******   MOVE    "2"             TO  FLG-TOROKU
               MOVE    SPACE           TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE           TO  FLG-TOROKU
           END-IF
      *
           .
       410121-HKNCOMBI-SAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       410132-SAIHENSYU-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           MOVE    1                   TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
      *
           INITIALIZE                  SPA-SCR-REC
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    画面用スパ編集処理
           MOVE    ZERO                TO  FLG-TEISEI
           PERFORM                 310-SPA-GMNSET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       410132-SAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
      *    診療科変更あり
           IF      K02N-SRYKA          =   SPA-GMN-SRYKA-G
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    訂正の時、エラー
           IF      SPA-SYORIFLG        =   1
      *********MOVE    "1008"              TO  SPA-ERRCD
               CONTINUE
           ELSE
               IF      SPA-PTID            =   ZERO
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   MOVE    K02N-SRYKA          TO  SPA-GMN-SRYKA-G
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    K02N-SRYKA          TO  SPA-GMN-SRYKA-G
      *    診療科
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    科が無い時、前の科に戻す
           IF      FLG-CHK             =   ZERO
               MOVE    SPA-MAE-SRYKA-G TO  SPA-GMN-SRYKA-G
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    訂正で既に同じ受診履歴がある時、エラー
           IF      SPA-SYORIFLG        =   1
               PERFORM 410123-JYURRK-HKNCHK-SEC
               IF      FLG-RRK-ARI         =   1
                   MOVE    "0128"              TO  SPA-KIDCD
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )
               IF      SPA-SYORIFLG        =   1
                   MOVE    "0123"              TO  SPA-KIDCD
               ELSE
                   IF      SPA-GMN-MAX         >   ZERO
                       MOVE    "0107"              TO  SPA-KIDCD
                   ELSE
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   END-IF
               END-IF
           END-IF
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE-SEC         SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
           MOVE    0                   TO  SPA-GMN-FLG
      *    対象の科の保険組合せにする
           IF      SPA-SYORIFLG        =   ZERO
      *        診療内容を残さない時、展開中のクリア
               IF      SPA-KID1-FLG        =   "OK"
                   MOVE    1               TO  SPA-GMN-FLG
               END-IF
      *        保険組合せ変更判定
               MOVE    SPA-K02             TO  HZN-SPA-K02
               MOVE    SPA-K01KYOUTU       TO  HZN-SPA-K01KYOUTU
               MOVE    SPA-AREA            TO  WRK-SPA-AREA
               MOVE    SPACE               TO  WRK-SPA-HKNCOMBINUM
               MOVE    SPACE               TO  WRK-SPA-DRCD
               MOVE    SPACE               TO  HZN-SPA-DRCD-NAME
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
               MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
               MOVE    "L"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           WRK-SPA-AREA
                                           HZN-SPA-K01KYOUTU
                                           HZN-SPA-K02
               IF      SPA-HKNCOMBINUM     =   WRK-SPA-HKNCOMBINUM
      *            保険変更がない時、処理を行う
                   MOVE    SPACE               TO  SPA-KID1-FLG
                   MOVE    WRK-SPA-DRCD        TO  SPA-DRCD
                   MOVE    HZN-SPA-DRCD-NAME   TO  SPA-DRCD-NAME
                   PERFORM 41013-SRYKA-CHANGE02-SEC
               ELSE
                   MOVE    "0138"              TO  SPA-KIDCD
               END-IF
           ELSE
               PERFORM 41013-SRYKA-CHANGE02-SEC
           END-IF
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE02-SEC         SECTION.
      *
      *    対象の科の保険組合せにする
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-KID1-FLG        =   "OK"
                   MOVE    SPACE               TO  SPA-HKNCOMBINUM
               END-IF
               MOVE    SPACE               TO  SPA-DRCD
      *        診療内容を残さない時、展開中のクリア
      ***      IF      SPA-KID1-FLG        =   "OK"
               IF      SPA-GMN-FLG         =   1
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-MOD-FLG
                   PERFORM 4900-WKSRYACT-CLEAR-SEC
               END-IF
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           MOVE    "K"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    再表示編集処理
           MOVE    SPACE               TO  WRK-KIDCD
           IF      LNK-SC95-WKSRYACT   =   1
               MOVE    "0122"          TO  WRK-KIDCD
           END-IF
      *
      *    保険組合
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           PERFORM 41012-HKNCOMBI-CHANGE-SEC
           IF      WRK-KIDCD       NOT =   SPACE
               MOVE    WRK-KIDCD           TO  SPA-KIDCD
           END-IF
      *
           MOVE    HZN-SCR-REC         TO  SPA-SCR-REC
           IF     (SPA-SYORIFLG        =   ZERO )  AND
                  (SPA-GMN-FLG         =   ZERO )
               INITIALIZE                  SPA-SCR-REC
           END-IF
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           .
       41013-SRYKA-CHANGE02-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外入力処理
      *****************************************************************
       41019-JIKANKBN-SYORI-SEC         SECTION.
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *
           MOVE    K02N-JIKANKBN       TO  SPA-K02N-JIKANKBN-G
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-K02N-JIKANKBN   =   SPA-K02N-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-K02N-JIKANKBN-LST (IDX)
                                           TO  SPA-K02N-JIKANKBN-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    5                TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "9002"              TO  SPA-ERRCD
               GO      TO       41019-JIKANKBN-SYORI-EXT
           END-IF
      *    時間外特例の時、システム管理にあること
           IF      SPA-K02N-JIKANKBN   =   "4"
               IF      SPA-JIKANTOKU   NOT =   1
                   MOVE    "0069"          TO  SPA-ERRCD
               END-IF
           END-IF
           MOVE    SPA-K02N-JIKANKBN   TO  SPA-NAI-TIMEFLG
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    画面→ＳＰＡ編集処理
           PERFORM 410110-INPUT-GMNSET-SEC
      *    入力コード・名称チェック処理
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    1                   TO  WRK-STR
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       41019-JIKANKBN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-ALLHEN-SEC               SECTION.
      *
      *    患者の入力チェック
           IF      SPA-PTID            =   ZERO
               IF      K02N-INPUTCD(1)     NOT =   SPACE
                   MOVE    00              TO  SPA-GMN-CUR
                   MOVE    "1005"          TO  SPA-ERRCD
                   GO      TO      41011-KOUI-ALLHEN-EXT
                END-IF
           END-IF
      *    入院期間チェック
           IF     (SPA-K02N-NYUINYMD   =   SPACE )
              OR  (SPA-K02N-NYUINYMD   >   SPA-SRYYMD )
              OR  (SPA-K02N-TAIINYMD   <   SPA-SRYYMD )
               IF      K02N-INPUTCD(1)     NOT =   SPACE
                   MOVE    00              TO  SPA-GMN-CUR
                   MOVE    "9001"          TO  SPA-ERRCD
                   GO      TO      41011-KOUI-ALLHEN-EXT
                END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    画面→ＳＰＡ編集処理
           PERFORM 410110-INPUT-GMNSET-SEC
      *
      *    入力コード・名称チェック処理
      *****PERFORM 410112-KOUI-SPACHK-SEC
      *
      *!!!!!!!!!!!!!!!!!!
      *!!!!!!!!!!!!!!!!!!
      *!!  連続挿入
           IF     (FLG-INPUT-NASI2      =   1  )  AND
                  (SPA-RENZ-FLG         =   1  )
               MOVE    ZERO                TO  SPA-RENZ-FLG
               MOVE    ZERO                TO  FLG-INSEND
               PERFORM VARYING     IDX     FROM   1    BY  1
                       UNTIL     ( IDX     >   SPA-MAX-LINE)
                           OR    ( FLG-INSEND  =   1       )
                   IF     (SPA-COM-GMNINS (IDX)    =   "1" )
                   AND    (SPA-GMN-INPUTCD (IDX)   =  SPA-RENZ-INPUT)
                       MOVE    1                   TO  FLG-INSEND
                       IF      SPA-GMN-INPUTCD (IDX - 1)
                                              NOT  =   SPACE
                           MOVE    IDX                 TO  SPA-GMN-IDX
                           MOVE    1                   TO  WRK-INSCNT
                           PERFORM 41014-GYOINSN-SEC
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "3"             TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                               ADD     1               TO  SPA-GMN-IDX
                               MOVE    1               TO  SPA-RENZ-FLG
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF      SPA-RENZ-FLG        =   ZERO
                   MOVE    SPACE           TO  SPA-RENZ-INPUT
                   PERFORM VARYING     IDX     FROM   1    BY  1
                           UNTIL     ( IDX     >   SPA-MAX-LINE)
                       MOVE    SPACE           TO  SPA-COM-GMNINS (IDX)
                   END-PERFORM
               ELSE
                   GO      TO  41011-KOUI-ALLHEN-EXT
               END-IF
           END-IF
      *!!!!!!!!!!!!!!!!!!
      *
      *
           IF      FLG-INPUT-NASI      =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
      *        カーソル位置
               PERFORM 410119-SAIINPUT-SEC
           ELSE
      *        入力コード・名称チェック処理
               PERFORM 410112-KOUI-SPACHK-SEC
      *
           END-IF
      *
           .
       41011-KOUI-ALLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面→ＳＰＡ編集処理
      *****************************************************************
       410110-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
           IF      SPA-NAI-WKSRYERR    =   1
               MOVE    ZERO                TO  FLG-INPUT-NASI
               MOVE    ZERO                TO  SPA-NAI-WKSRYERR
           ELSE
               MOVE    1                   TO  FLG-INPUT-NASI
           END-IF
      *!!!!!!!!!!!!!!!!!!
           MOVE    1                   TO  FLG-INPUT-NASI2
      *!!!!!!!!!!!!!!!!!!
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-CUR (IDX)   =   "1"  OR  "2"
                   IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                       MOVE    ZERO                TO  FLG-INPUT-NASI
                   END-IF
               END-IF
      *        数量チェック
               IF      SPA-COM-SURCHK (IDX)    =   "2"
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
               MOVE    SPACE               TO  SPA-COM-IN4 (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-STR
           MOVE    ZERO                TO  FLG-KUHAKU
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL      (WRK-IDX2        >   MAX-GMN   )
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (K02N-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (K02N-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   MOVE    ZERO                TO  FLG-INPUT-NASI
      *!!!!!!!!!!!!!!!!!!
                   MOVE    ZERO                TO  FLG-INPUT-NASI2
      *!!!!!!!!!!!!!!!!!!
               END-IF
      *ver.4.7
      *        今回変更位置
               IF     (K02N-INPUTCD (WRK-IDX2) NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX))
               AND    (K02N-INPUTCD(WRK-IDX2 + 1) NOT =   SPACE )
                   MOVE    "1"                 TO  SPA-COM-IN4
                                                           (SPA-GMN-IDX)
               END-IF
               IF     (K02N-MEISYO  (WRK-IDX2) NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
               AND    (K02N-INPUTCD(WRK-IDX2 + 1) NOT =   SPACE )
      *H30.9
               AND   ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                               =   "8"   )  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                               =   "008"))
                   MOVE    "2"                 TO  SPA-COM-IN4
                                                           (SPA-GMN-IDX)
               END-IF
      *ver.4.7
      *
               IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                           NOT =   SPA-MAE-INPUTCD
                                                         (SPA-GMN-IDX))
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
      *
               MOVE    K02N-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    K02N-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   IF     (K02N-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      WRK-STR             =   ZERO
                   MOVE    SPA-GMN-IDX         TO  WRK-STR
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN-IDX     >=  SPA-MAX-LINE )  AND
                  (WRK-IDX2        <=  MAX-GMN  )
               PERFORM VARYING     WRK-IDX3    FROM    WRK-IDX2 BY  1
                       UNTIL      (WRK-IDX3    >   MAX-GMN  )   OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      K02N-INPUTCD (WRK-IDX3) NOT =  SPACE
                       MOVE    "7004"          TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
           .
       410110-INPUT-GMNSET-EXT.
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
               IF      SPA-GMN-IDX     >   SPA-MAX-LINE
                   MOVE    "7001"          TO  SPA-ERRCD
                   MOVE    SPA-MAX-LINE        TO  SPA-GMN-IDX
               END-IF
           END-IF
      *
           .
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC               SECTION.
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
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
      *!!!
      *    回数行の複写
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   SPA-MAX-LINE
               IF     (SPA-GMN-INPUTCD (IDX1)(1:1)  =   "*" )
                  AND (SPA-GMN-INPUTCD (IDX1)(2:1)  =   "C"
                                                   OR  "c" )
                  AND (SPA-GMN-INPUTCD (IDX1)(3:1)  =   "A"
                                                   OR  "a" )
                   PERFORM 51001-KAISU-INS-SEC
               END-IF
           END-PERFORM
      *    回数行の複写
           MOVE    ZERO                TO  IDX-KAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "*"
                   PERFORM 51000-ALLKAISU-HEN-SEC
               END-IF
           END-PERFORM
      *!!!
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           MOVE    SPACE               TO  WRK-ERRCD2
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   SPA-MAX-LINE)  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
      *??      OR     (SPA-GMN-INPUTCD (SPA-GMN-IDX)   NOT =   SPACE)
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *        フリーコメントの名称の全角チェックはすべて行う
               IF     (SPA-ERRCD           =   SPACE )
                   IF    ((SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "810000001" ) OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "008500000" ) OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                           =   "83" OR "84") OR
      *R02.3
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =  "85"        ) OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                           =   "0083" 
                                           OR  "0084"
                                           OR  "0085"
                                           OR  "0086"    ))  AND
                          (SPA-GMN-MEISYO-G(SPA-GMN-IDX) 
                                       NOT =   SPACE  )
                       PERFORM 401131-MEISYO-HENKAN-SEC
                   END-IF
               END-IF
               IF     (SPA-ERRCD       =   SPACE )  AND
                      (FLG-END         =   ZERO  )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    数量ゼロエラー
           IF      WRK-ERRCD2      NOT =   SPACE
               MOVE    WRK-ERRCD2          TO  SPA-ERRCD
           END-IF
      *
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
           IF     (FLG-INS             =   1     )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    点数計算・剤別チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           IF     (SPA-ERRCD       =   SPACE
                                   OR  "K839"
                                   OR  "0021" )  AND
                  (SPA-KIDCD       =   SPACE  )  AND
                  (FLG-END         =   ZERO   )
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
           END-IF
      *    他画面遷移時
           IF      FLG-HENKOU          =   1
      *H24.10
      *        警告はクリア
               IF      SPA-ERRCD (1:1)     =   "K"
                   PERFORM 410129-KEI-INIT-SEC
               END-IF
               MOVE    SPACE           TO  SPA-KIDCD
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    ZERO            TO  FLG-END
           END-IF
      *
           .
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *H24.10
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       410129-KEI-INIT-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
      *
               IF      SPA-COM-CUR (IDX)   =   "1"   OR  "2"
      *        警告フラグクリア
                   EVALUATE    SPA-ERRCD
                   WHEN    "K236"
                   WHEN    "K111"
                   WHEN    "K112"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG2(IDX)
                   WHEN    "K839"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG3(IDX)
                   WHEN    "K200"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG4(IDX)
                   WHEN    "K201"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG5(IDX)
                   WHEN    "K122" 
                       MOVE    SPACE           TO  SPA-COM-KZMFLG6(IDX)
      *            入院料併用チェック
                   WHEN    "K910"
                       MOVE    SPACE           TO  SPA-COM-KZMFLGNK(IDX)
      *H29.1       入院料併用チェック
                   WHEN    "K190"
                       MOVE    SPACE           TO  SPA-COM-KZMFLGN2(IDX)
                   WHEN    "K227"
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1
                                                                  (IDX)
                   WHEN    "K228"
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG
                                                                  (IDX)
                   WHEN    "K210"
                   WHEN    "K211"
                   WHEN    "K212"
                   WHEN    "K213"
                       MOVE    SPACE           TO  SPA-COM-KINKIFLG
                                                                  (IDX)
                   WHEN    OTHER
                      MOVE    SPACE            TO  SPA-COM-KZMFLG (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       410129-KEI-INIT-EXT.
           EXIT.
      *H24.10
      *
      *!!!
      *!!!
      *****************************************************************
      *    すべての剤回数追加処理
      *****************************************************************
       51000-ALLKAISU-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  FLG-COPY
           PERFORM VARYING     IDX3    FROM    2   BY  1
                   UNTIL      (IDX3        >   CONS-INPUT  )
                         OR   (FLG-COPY    =   1   )
               IF      SPA-GMN-INPUTCD (IDX)(IDX3:1)
                                          =   "C"  OR  "c"
                   MOVE    1              TO  FLG-COPY
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)(IDX3:1)
                                          =   "/"
                   MOVE    IDX3           TO  IDX2
               END-IF
           END-PERFORM
      *
           IF      FLG-COPY            =   1
               IF      IDX-KAI         >   ZERO
                   IF      IDX2            =   ZERO
      *            行複写
                       MOVE    SPA-GMN-INPUTCD (IDX-KAI)
                                               TO   SPA-GMN-INPUTCD(IDX)
                   ELSE
      *            日付のみ複写
                       MOVE    SPA-GMN-INPUTCD (IDX-KAI)(IDX-KAI2:)
                                               TO   SPA-GMN-INPUTCD(IDX)
                                                         (IDX2:)
                   END-IF
               END-IF
           ELSE
               IF      IDX2            >   ZERO
                   MOVE    IDX             TO  IDX-KAI
                   MOVE    IDX2            TO  IDX-KAI2
               END-IF
           END-IF
           .
       51000-ALLKAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    すべての剤回数追加処理
      *****************************************************************
       51001-KAISU-INS-SEC               SECTION.
      *
           MOVE    "*KIJUN"            TO  SPA-MAE-INPUTCD (IDX1)
      *
           IF      SPA-GMN-INPUTCD (IDX1 - 1)(1:1)
                                       =   "*"
               MOVE    SPACE               TO  SPA-GMN-INPUTCD (IDX1)
           ELSE
               MOVE    SPA-COM-KAISU  (IDX1 - 1)   TO  WRK-KAISU-Z
               PERFORM 510011-KAISU-HEN-SEC
               MOVE    WRK-INPUTCD         TO  SPA-GMN-INPUTCD (IDX1)
           END-IF
      *
           MOVE    ZERO                TO  FLG-COPYOK
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   SPA-MAX-LINE
               IF     (SPA-GMN-INPUTCD (IDX3)(1:1) =   "*" )
                   MOVE    1               TO  FLG-COPYOK
               END-IF
               IF     (SPA-COM-ZAIEND  (IDX3)      =   "1" )
               AND    (SPA-GMN-INPUTCD (IDX3)(1:1) NOT =   "*" )
                   IF     (SPA-GMN-INPUTCD (IDX3 + 1)(1:1)
                                                       =   "*" )
                   OR     (FLG-COPYOK                  =   ZERO  )
                       CONTINUE
                   ELSE
                       IF     (IDX3                <   SPA-MAX-LINE )
                           MOVE    SPA-COM-KAISU (IDX3)
                                                       TO  WRK-KAISU-Z
                           PERFORM 510011-KAISU-HEN-SEC
                           COMPUTE SPA-GMN-IDX         =   IDX3  +  1
                           PERFORM 41014-GYOINSN-SEC
                           IF      SPA-ERRCD           =   SPACE
                               MOVE    WRK-INPUTCD     TO
                                                       SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
                               ADD     1               TO  IDX3
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        終了行判定
               IF      SPA-MAE-INPUTCD (IDX3 + 1)  =   "*KIJUN"
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                             (IDX3 + 1)
                   MOVE    SPA-MAX-LINE        TO  IDX3
               END-IF
           END-PERFORM
           .
       51001-KAISU-INS-EXT.
           EXIT.
      *****************************************************************
      *    回数複写処理
      *****************************************************************
       510011-KAISU-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    SPACE               TO  WRK-KAISU-H
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
           IF      WRK-KAISU-H         =   SPACE
                                       OR  "1"
               MOVE    "*/C"                TO  WRK-INPUTCD
           ELSE
               STRING  "*"                 DELIMITED  BY  SIZE
                       WRK-KAISU-H         DELIMITED  BY  SPACE
                       "/C"                DELIMITED  BY  SIZE
                                           INTO    WRK-INPUTCD
               END-STRING
           END-IF
           .
       510011-KAISU-HEN-EXT.
           EXIT.
      *!!!
      *!!!
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
      *    ＰＧ名
           MOVE    "K02N"              TO  LNK-SC97-PG
           MOVE    "1"                 TO  LNK-SC97-KBN
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
           EVALUATE    LNK-SC97-INPUTCD(1:1)
           WHEN    SPACE
      *        削除時
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                       ((SPA-GMN-IDX           <   SPA-MAX-LINE  ) AND
                        (SPA-COM-SET  (SPA-GMN-IDX + 1)    =   "1"))
                   MOVE    SPA-GMN-IDX         TO  IDX
                   MOVE    SPACE       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
                   PERFORM 410151-SETDEL2-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
      *
      *    剤削除時
           WHEN    "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41015-ZAIDEL-SEC
               MOVE    1               TO  FLG-INS
           END-EVALUATE
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
      *    ＰＧ名
           MOVE    "K02N"              TO  LNK-SC97-PG
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
      *H25.4
               MOVE    SPACE           TO  SPA-COM-IN4 (SPA-GMN-IDX)
      *        挿入行数
               MOVE    LNK-SC97-INSCNT TO  WRK-INSCNT
      *        約束セット前は１行
               IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "S"  )  AND
                      (LNK-SC97-INSCNT     >   1    )
                   MOVE    1               TO  WRK-INSCNT
               END-IF
      *
      *!!!!!!!!!!!!!!!!!!!
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   ADD     1               TO  SPA-GMN-IDX
                   MOVE    1               TO  FLG-INS
      *!!!!!!!!!!!!!!
      *            連続挿入
                   IF     (SPA-RENZ-FLG        =   ZERO )
                   AND    (LNK-SC97-INSREN     =   1    )
                       MOVE    1               TO  SPA-RENZ-FLG
                       MOVE    "1"             TO  SPA-COM-GMNINS
                                                      (SPA-GMN-IDX)
                       MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  SPA-RENZ-INPUT
                   END-IF
               END-IF
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    一覧へ
           IF      LNK-SC97-INPUTCD(1:2)
                                       =  "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-K98-AREA
               MOVE    LNK-SC97-INPUTCD    TO  SPA-K98-INPUTCD
               PERFORM 410-K98-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *H28.5
      *    院外投薬名称変換
           IF     (LNK-SC97-INPUTCD(1:2)
                                       =   "/G"
                                       OR  "/g" )
            AND   (LNK-SC97-INPUTCD(3:)    =   SPACE )
            AND   (SPA-SRYYMD          >=  "20160401")
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      SPA-GENERICFLG      =   "1"
                   MOVE    "0"             TO  SPA-GENERICFLG
               ELSE
                   MOVE    "1"             TO  SPA-GENERICFLG
               END-IF
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *TEST
      *    Ｐ入力へ
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-K99-SYORI-SEC
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
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                     ((SPA-GMN-IDX           <   SPA-MAX-LINE  ) AND
                      (SPA-COM-SET  (SPA-GMN-IDX + 1)      =   "1"))
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
                   MOVE    SPA-COM-TUKIJGNFLG (SPA-GMN-IDX)
                                               TO  WRK-TUKIJGNFLG
                   MOVE    SPA-COM-TUKIJGNFLG1(SPA-GMN-IDX)
                                               TO  WRK-TUKIJGNFLG1
               ELSE
                   MOVE    SPACE               TO  WRK-KINKIFLG
                   MOVE    ZERO                TO  WRK-PTKINKI-ARI
                   MOVE    SPACE               TO  WRK-TUKIJGNFLG
                   MOVE    SPACE               TO  WRK-TUKIJGNFLG1
               END-IF
      *
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)    TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)    TO  WRK-FLG-IN
               MOVE    SPA-COM-IN4    (SPA-GMN-IDX)    TO  WRK-FLG-IN4
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    WRK-FLG-IN4     TO  SPA-COM-IN4 (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
               MOVE    WRK-KINKIFLG    TO  SPA-COM-KINKIFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-PTKINKI-ARI TO  SPA-COM-PTKINKI-ARI
                                                        (SPA-GMN-IDX)
               MOVE    WRK-TUKIJGNFLG  TO  SPA-COM-TUKIJGNFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-TUKIJGNFLG1 TO  SPA-COM-TUKIJGNFLG1
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
               INITIALIZE                      SPA-K98-AREA
               MOVE    LNK-SC97-INCD       TO  SPA-K98-INPUTCD
               MOVE    "1"                 TO  SPA-K98-TYPE
               MOVE    LNK-SC97-CDSYU      TO  SPA-K98-CDSYUBETU
               MOVE    "3"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-K98-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               MOVE    SPA-COM-IN4    (SPA-GMN-IDX)    TO  WRK-FLG-IN4
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-FLG-IN4     TO  SPA-COM-IN4 (SPA-GMN-IDX)
               MOVE    LNK-SC97-INPUTCD(1:4)   TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO
                                                    (SPA-GMN-IDX)
               MOVE    "*"                 TO  SPA-GMN-MEIH
                                                    (SPA-GMN-IDX)
               MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN
                                                    (SPA-GMN-IDX)
           ELSE
      *        文字変換・編集処理
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 410112-INPUTCD-HENKAN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HENKOU          =   1
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
      *    数量ゼロエラーはエラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPA-ERRCD           TO  WRK-ERRCD2
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-K99-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-K99GMN-AREA
           MOVE    SPACE               TO  K99
      *
      *    画面カーソルセット処理
           INITIALIZE                      K99
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  SPA-K99-DATA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K99"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-K99-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-K99-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-K98-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KOUI
      *
           INITIALIZE                  SPA-K98GMN-AREA
      *****IF      SPA-K98-INPUTCD         =   SPACE
           IF      SPA-K98-INPUTCD(1:2)    =   "//"
               MOVE    SPA-K98-INPUTCD     TO  SPA-K98-GMN-INPUT
               EVALUATE    SPA-K98-INPUTCD(3:1)
      *        コメント一覧
               WHEN    "C"
               WHEN    "c"
                   MOVE    "U"                 TO  SPA-K98-GMN-SYORI
                   MOVE    "U4"                TO  SPA-K98-GMN-SYORI3
                   MOVE    SPACE               TO  SPA-K98-INPUTCD
      *        用法
               WHEN    "Y"
               WHEN    "y"
                   MOVE    "U"                 TO  SPA-K98-GMN-SYORI
                   MOVE    "U1"                TO  SPA-K98-GMN-SYORI3
                   MOVE    SPACE               TO  SPA-K98-INPUTCD
      *        診療種別一覧
               WHEN    "."
                   MOVE    "."                 TO  SPA-K98-INPUTCD
      *        区分番号別
               WHEN    "*"
               WHEN    "/"
                   MOVE    SPA-K98-GMN-INPUT(3:)   TO  SPA-K98-INPUTCD
      *H25.6   労災一覧
               WHEN    "R"
               WHEN    "r"
                   MOVE    SPA-K98-INPUTCD     TO  SPA-K98-GMN-INPUT
      *
      *H30.9
      *        選択式コメント一覧
               WHEN    "S"
               WHEN    "s"
                   IF      SPA-SRYYMD      <   "20180401"
                       MOVE    "1107"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-K98-GMN-INPUT
                   ELSE
                       PERFORM 41019-K98-RECEKISAI-SEC
                   END-IF
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO  410-K98-SYORI-EXT
                   END-IF
      *
               WHEN    OTHER
                   MOVE    SPACE               TO  SPA-K98-INPUTCD
               END-EVALUATE
      *        画面編集
               PERFORM 4101-K98-SET-SEC
           ELSE
               INITIALIZE                  SPA-K98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-K98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)   =   "S")
      *            内服薬剤
                   MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   MOVE    1                   TO  FLG-KOUI
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(SPA-GMN-IDX - 1)
                       WHEN    SPACE
      *                    診療区分が決定してない時
                           PERFORM  4101-K98-SYRKBN-SEC
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
      *                    注射
                           MOVE    "5"         TO  SPA-K98-GMN-SYORI
                       WHEN    "23"
      *                    外薬
                           MOVE    "4"         TO  SPA-K98-GMN-SYORI
                       WHEN    "21"
                       WHEN    "22"
      *                    内服
                           MOVE    "3"         TO  SPA-K98-GMN-SYORI
                       WHEN    OTHER
      *                    診療行為
      *D                   MOVE    "8"         TO  SPA-K98-GMN-SYORI
                           MOVE    "3"         TO  SPA-K98-GMN-SYORI
                           MOVE    1           TO  FLG-KOUI
                   END-EVALUATE
               END-IF
      *        画面編集
               PERFORM 4101-K98-SET-SEC
      *
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-K98-GMN-MAX     =   ZERO )  AND
                      (SPA-K98-CDSYUBETU   =   "J"  )  AND
                      (FLG-KOUI            =   1    )
                   INITIALIZE                  SPA-K98GMN-AREA
                   MOVE    "8"                 TO  SPA-K98-GMN-SYORI
      *            画面編集
                   PERFORM 4101-K98-SET-SEC
               END-IF
           END-IF
      *
      *    エラークリア
           MOVE    SPACE               TO  SPA-ERRCD
      *    画面カーソルセット処理
           IF      SPA-K98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K98"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-K98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410-K98-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分が決定してない時診療種別処理
      *****************************************************************
       4101-K98-SYRKBN-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IDX
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:3)
                   WHEN    ".21"
                       MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".22"
                       MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".23"
                       MOVE    "4"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".31"
                   WHEN    ".32"
                   WHEN    ".33"
                       MOVE    "5"                 TO  SPA-K98-GMN-SYORI
               END-EVALUATE
           END-PERFORM
      *
           .
      *
       4101-K98-SYRKBN-EXT.
           EXIT.
      *
      *H30.9
      *****************************************************************
      *    選択式コメント一覧編集処理
      *****************************************************************
       41019-K98-RECEKISAI-SEC       SECTION.
      *
           IF       SPA-K98-INPUTCD(4:1)   =   "S"
                                           OR  "s"
               MOVE    "R"             TO  SPA-K98-GMN-SYORI
               MOVE    SPACE           TO  SPA-K98-INPUTCD
               MOVE    SPACE           TO  SPA-K98-GMN-SYORI2
               MOVE    "2"             TO  SPA-K98-TYPE
           ELSE
               MOVE    "C"             TO  SPA-K98-GMN-SYORI
               MOVE    SPACE           TO  SPA-K98-INPUTCD
               IF     (SPA-SELCOM-IDX  =   ZERO )
                 AND  (SPA-GMN-IDX     >   1    )
                   PERFORM 510112-SELCOM-CHK-SEC
               END-IF
               IF     (SPA-SELCOM-IDX  =   ZERO  )
                   MOVE    SPACE           TO  SPA-K98-GMN-SYORI
                   MOVE    SPACE           TO  SPA-K98-INPUTCD
                   MOVE    "1106"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-COM-INPUTCD (SPA-SELCOM-IDX)
                                               TO  SPA-K98-SELCOM-SRYCD
               END-IF
           END-IF
           .
       41019-K98-RECEKISAI-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-K98-SET-SEC       SECTION.
      *
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           .
      *
       4101-K98-SET-EXT.
           EXIT.
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
      *        '_'をなくす
               INSPECT SPA-GMN-INPUTCD (IDX)
                       REPLACING   ALL "_"  BY  " "
      *        ＊をなくす
               PERFORM 51011-KAISU-CHK-SEC
      *
      *        薬評・器評の数量なしとする
               IF     (SPA-COM-IN2     (IDX)   =   "2" )
                  AND (SPA-COM-YAKUHYO (IDX)   =   "1" )
                   IF    (SPA-COM-INPUTCD (IDX)(1:1)  =   "6"
                                                      OR  "7" )
                     OR  (SPA-COM-INPUTCD (IDX)(1:3)  =   "059" )
                       MOVE    SPACE           TO  SPA-COM-IN2 (IDX)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF     (FLG-INPUT-NASI      =   1  )  AND
                  (FLG-KUHAKU          =   1  )
               MOVE    ZERO                TO  WRK-IN-IDX
           END-IF
      *
           IF      WRK-IN-IDX          =   ZERO
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
      *    他画面遷移時カーソル移動なし
           IF      FLG-HENKOU          =   1
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *
      *    数量・回数入力なし
           IF     (SPA-GMN-IDX             >   ZERO        ) AND
                  (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "3"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001")
                  AND (SPA-COM-YCOMKBN (SPA-GMN-IDX)       =   ZERO )
      *            投薬の時のみ、回数入力へ
                   IF      SPA-COM-SRYKBN(SPA-GMN-IDX)     =   "21" OR
                                                               "22" OR
                                                               "23"
                       STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
                               "*"              DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                       END-STRING
                   ELSE
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
               ELSE
                   STRING  WRK-INPUTCD          DELIMITED  BY  SPACE
                           "_"                  DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   END-STRING
               END-IF
               IF      SPA-ERRCD           =   "0021"
      *            数量下限エラーはなし
                   MOVE    SPACE                TO  SPA-ERRCD
               END-IF
           END-IF
      *    名称入力へのカーソル移動
           IF      WRK-IN-IDX          >   ZERO
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                           =   "810000001" ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                           =   "008500000" ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83") OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083") 
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0085") 
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0086") 
      *R02.4
                   IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                           =   "831"       )
                       CONTINUE
                   ELSE
                       MOVE    "3"         TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF     (FLG-KUHAKU          =   1  )  AND
                  (SPA-GMN-IDX         >   ZERO)
               IF     (SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                               NOT =   "."  )  AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX)    =   SPACE )
      *            回数入力へ
                   MOVE    ZERO                TO  WRK-IN-IDX2
                   MOVE    ZERO                TO  FLG-ARI
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   54
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                                   =   "*"
                           MOVE    1               TO  FLG-ARI
                       END-IF
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                               NOT =  SPACE
                           MOVE    IDX             TO  WRK-IN-IDX2
                       END-IF
                   END-PERFORM
                   IF     (FLG-ARI                 =   ZERO )  AND
                          (WRK-IN-IDX2             <   54   )
                       ADD     1                   TO  WRK-IN-IDX2
                       IF    ((SPA-COM-IN3 (SPA-GMN-IDX)  =   "1" ) AND
                              (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                      NOT =   "001"))
                         OR  ((SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                                       =   "S" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                                       =   "84" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                                       =   "0084") AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *                  用量コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992000" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      **                 自賠責金額入力
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0959"   ) AND
                              (SPA-COM-SRYSYUKBN(SPA-GMN-IDX)
                                                       =   "809" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *                  用法コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001"     ) AND
                              (SPA-COM-YCOMKBN(SPA-GMN-IDX) =   1 ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *H30.4             分割調剤など
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992081" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *R02.4
      *        新コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "85"   ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                       =   "831" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *
                           MOVE    "_"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
                       END-IF
                   END-IF
               END-IF
      *        空白行入力時、最終行へ
               MOVE    "3"                 TO  SPA-COM-CUR
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
      *    回数入力
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                       =   "*"
               PERFORM 4101129-KAISUU-CHK-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
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
      *
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
      *    入力値の入力判定用
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4
           MOVE    SPA-COM-ATAI5 (SPA-GMN-IDX) TO  WRK-HZN-ATAI5
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX)
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "5"                 TO  LNK-SC92-KBN
           END-IF
           IF      FLG-HENKOU          =   1
      *        画面遷移前
               IF     (LNK-SC97-NYURYOKU       =   1 ) OR
                      (SPA-COM-CHKFLG (SPA-GMN-IDX)  =   SPACE)
                   MOVE    "6"                 TO  LNK-SC92-KBN
               END-IF
           END-IF
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
      *
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *
           MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *    カーソル位置設定
           PERFORM 41011139-INFLG-HEN-SEC
      *
      *    剤内に入院回数がある時、併用算定と算定チェックをしない
      **** IF      SPA-COM-NYU-KAI  (SPA-GMN-IDX)
      ***                          NOT =   ZERO
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPACE           TO  LNK-SC92-ERRCD(3)
               MOVE    SPACE           TO  LNK-SC92-ERRCD(6)
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  AND
                  (LNK-SC92-ERRMSG NOT =   SPACE )
               MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
           END-IF
      *
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
                   IF      IDX-ERR             =   3   OR  6
                       MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
                   END-IF
               END-IF
           END-PERFORM
      *
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI4   =   SPA-COM-ATAI4 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI5   =   SPA-COM-ATAI5 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF
      *
      *入院は小児科外来診察料なし
      *    小児科外来診療科の時、年齢エラーはそのまま
      *    IF     (SPA-SYONIKA-ARI     NOT =   ZERO )  AND
      *           (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)  =   "1" )
      *        IF      SPA-ERRCD           =   "0032"
      *            MOVE    SPACE               TO  SPA-ERRCD
      *        END-IF
      **** END-IF
      *---(01.00.01) LINE ADD  START  ----
      *    警告判定
           IF      SPA-ERRCD(1:1)      =   "K"
               IF      SPA-COM-KZMFLG (SPA-GMN-IDX)  =   SPACE
                   MOVE    "1"             TO  SPA-COM-KZMFLG
                                                          (SPA-GMN-IDX)
                   MOVE    "2"             TO  SPA-COM-CUR(SPA-GMN-IDX)
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-COM-CUR(SPA-GMN-IDX)
               END-IF
           END-IF
      *---(01.00.01) LINE ADD  END    ----
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-HCHKFLG (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      **** 時間外加算の時、ＳＰＡへ
      *    IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)  NOT =   ZERO
      *        IF      SPA-SYORIFLG        =   ZERO
      *            MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)
      *                                    TO  SPA-NAI-TIMEFLG
      *        END-IF
      **** END-IF
      *
      *    保険適用区分が保険適用外のもの（ユーザー登録の自費分）
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
      *        数量を金額とする
      ****     IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
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
      *!!!         定額の時、数量入力エラー
      *            IF      WRK-SURYO       NOT =   ZERO
      *                MOVE    "1002"          TO  SPA-ERRCD
      *!!!         END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
           END-IF
      *
      *
           .
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *
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
                  (WRK-MCNT4           =   ZERO )  AND
                  (WRK-MCNT5           =   ZERO )  AND
                  (FLG-KAISU           =   ZERO )
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
      *        自賠責金額
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                               =   "0959" )
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
      *
      *R02.4
      *        新コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                               =   "85"  )
                  OR  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "831"  )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *
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
      *    入力文字を数字変換処理
      *****************************************************************
       4101121-MOJI-SET-SEC        SECTION.
      *
           MOVE    LNK-SC97-SURYOCNT   TO  WRK-SUR-MCNT
           MOVE    LNK-SC97-KAISUCNT   TO  WRK-ACCT-MCNT
           MOVE    LNK-SC97-SURYO      TO  WRK-SURYO
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    LNK-SC97-KAISUFLG   TO  FLG-KAISU
           MOVE    LNK-SC97-BUNGSU     TO  WRK-BUNGSU
           MOVE    LNK-SC97-MCNT1      TO  WRK-MCNT1
           MOVE    LNK-SC97-MCNT2      TO  WRK-MCNT2
           MOVE    LNK-SC97-MCNT3      TO  WRK-MCNT3
           MOVE    LNK-SC97-MCNT4      TO  WRK-MCNT4
           MOVE    LNK-SC97-MCNT5      TO  WRK-MCNT5
           MOVE    LNK-SC97-MOJI1      TO  WRK-MOJI1
           MOVE    LNK-SC97-MOJI2      TO  WRK-MOJI2
           MOVE    LNK-SC97-MOJI3      TO  WRK-MOJI3
           MOVE    LNK-SC97-MOJI4      TO  WRK-MOJI4
           MOVE    LNK-SC97-MOJI5      TO  WRK-MOJI5
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    セットの入力値
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
           MOVE    "K02"               TO  LNK-SC92-PG
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *
           .
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数入力チェック処理
      *****************************************************************
       4101129-KAISUU-CHK-SEC           SECTION.
      *
      *    回数に変更があった時、剤内の警告はクリアする
           IF     SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                   NOT =   SPA-MAE-INPUTCD(SPA-GMN-IDX)
      *H29.1
              IF      (SPA-COM-KZMFLGN2(SPA-GMN-IDX)   =   "1" )
                AND   (SPA-MAE-INPUTCD(SPA-GMN-IDX)    =   SPACE )
                  CONTINUE
              ELSE
      *
               MOVE    SPACE           TO  SPA-COM-KZMFLGN2(SPA-GMN-IDX)
               COMPUTE IDX-K           =   SPA-GMN-IDX   -   1
               PERFORM VARYING     IDX-K2  FROM    IDX-K  BY  -1
                       UNTIL       IDX-K2  <   1
                  IF      SPA-COM-ZAIEND (IDX-K2)  =   "1"
                      MOVE    1                TO  IDX-K2
                  ELSE
                      IF      SPA-COM-SRYKBN (IDX-K2)  =  "80"
                                                       OR "54"
                           MOVE    SPACE           TO  SPA-COM-KZMFLG
                                                            (IDX-K2)
                           MOVE    SPACE           TO  SPA-COM-KZMFLG2
                                                            (IDX-K2)
                           MOVE    SPACE           TO  SPA-COM-KZMFLG3
                                                            (IDX-K2)
                      END-IF
                      IF      SPA-COM-SRYKBN (IDX-K2)  =  "90"
                                                       OR "92"
                                                       OR "97"
                           MOVE    SPACE           TO  SPA-COM-KZMFLGNK
                                                            (IDX-K2)
                      END-IF
                  END-IF
               END-PERFORM
           END-IF
      *
           END-IF
      *
      *    クリア
           MOVE    SPACE           TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           MOVE    SPA-COM-IN4(SPA-GMN-IDX)    TO  WRK-FLG-IN4
           MOVE    SPA-COM-KZMFLGN2(SPA-GMN-IDX)   TO  WRK-KINKIFLG
      *H29.4   投薬警告保存
           IF      SPA-COM-KZMFLG (SPA-GMN-IDX) =  "1"
               MOVE    1               TO  FLG-KEIFLG
           ELSE
               MOVE    ZERO            TO  FLG-KEIFLG
           END-IF
           INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
           MOVE    WRK-FLG-IN4     TO  SPA-COM-IN4 (SPA-GMN-IDX)
           MOVE    WRK-KINKIFLG    TO  SPA-COM-KZMFLGN2 (SPA-GMN-IDX)
           IF      FLG-KEIFLG      =   1
               MOVE    "1"         TO  SPA-COM-KZMFLG (SPA-GMN-IDX)
           END-IF
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
      *        回数変更あり
               IF      WRK-KAISU       NOT =   SPA-COM-KAISU
                                                        (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-KAIFLG2
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (SPA-GMN-IDX)
      *    日付チェック
           IF      SPA-K02N-NYUINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-NYUINYMD(7:2)  TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-K02N-TAIINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-TAIINYMD(7:2)  TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-NAI-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
              IF      LNK-SC97-NYU-DAY (IDX)   NOT =   ZERO
                   IF     (IDX             >=  WRK-STDD )  AND
                          (IDX             <=  WRK-EDDD )
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (SPA-GMN-IDX IDX)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                       (SPA-GMN-IDX)
                   ELSE
                       MOVE    "9007"          TO  SPA-ERRCD
                   END-IF
      *            訂正の時、訂正日のみ
                   IF     (SPA-SYORIFLG        =   1   )  AND
                          (IDX             NOT =   WRK-SRYDD)
                       MOVE    "9008"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    保険組合せ期間との日付チェック
           IF      SPA-COB-TEKSTYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKSTYMD(7:2)   TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-COB-TEKEDYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKEDYMD(7:2)   TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-NAI-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   31  )
                          OR  (SPA-ERRCD   NOT =   SPACE )
              IF      LNK-SC97-NYU-DAY (IDX)   NOT =   ZERO
                   IF     (IDX             >=  WRK-STDD )  AND
                          (IDX             <=  WRK-EDDD )
                       CONTINUE
                   ELSE
                       MOVE    "9038"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-ERRMSG2
                       STRING  WRK-STDD
                               "日〜"
                               WRK-EDDD
                              "日が当月の有効期間です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG2
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4101129-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *    時間外がないこと
           IF      LNK-SC97-JIKAN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF
      *
      *
           MOVE    1                   TO  SPA-SINSATU-SAI
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410119-INPSET-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       410119-INPSET-EXT.
           EXIT.
      *
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
      *    入力位置クリア
      *?   MOVE    SPACE               TO  SPA-COM-IN4 (SPA-GMN-IDX)
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "K02"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
      *    ORCSC92検索用
           MOVE    SPA-SYORIFLG        TO  ORCSCSETHEN-SYORIFLG
           MOVE    SPA-NENREI          TO  ORCSCSETHEN-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  ORCSCSETHEN-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  ORCSCSETHEN-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  ORCSCSETHEN-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  ORCSCSETHEN-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  ORCSCSETHEN-TEISEI-AREA
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
      **** MOVE    1               TO  WRK-IDX-FT
      **** PERFORM 4101121-MOJI-SET-SEC
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
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC
           MOVE    WRK-CD(1:WRK-CD-MCNT)   TO  WRK-INPUTCD
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
           MOVE    SPA-AKUSEI-FLG      TO  FLG-MAE-AKUSEI
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "1"                 TO  ORCSC100-KBN
           MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
           MOVE    FLG-INIT            TO  ORCSC100-INIT
           MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
           MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
           CALL    "ORCSC100"          USING
                                       ORCSC100AREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    悪性検査一覧選択後のエラー
           IF     (FLG-MAE-AKUSEI      =   1   )
             AND  (SPA-ERRCD       NOT =   SPACE)
               GO      TO      510-TBLHEN-EXT
           END-IF
      *    悪性腫瘍検査一覧へ
           IF      SPA-AKUSEI-FLG      =   1
      *    中途データ展開時はなし
           AND    (FLG-INIT            =   ZERO  )
               PERFORM 5101-AKUSEI-K98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *
      *TEST
      *!!!!
      *H30.9
      *    選択コメント一覧へ
           IF     (SPA-SELCOM-FLG      =   1   )
      *    中途データ展開時はなし
           AND    (FLG-INIT            =   ZERO )
           AND    (SPA-SRYYMD          >=  "20180401")
               PERFORM 51011-SELCOM-K98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *TEST
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               OR (FLG-INIT            =   1      )
               GO      TO      510-TBLHEN-EXT
           END-IF
      *    禁忌チェックを行う
           IF      SPA-ERRCD       =   SPACE
           AND    (FLG-TOROKU          =   SPACE ) 
               PERFORM 51010-KNKCHK-SEC
               IF      FLG-KINKI           =   1
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF
      *!!!
      *    入院料包括の判定
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")  AND
      *            治験以外
                  (SPA-CHIKENKBN   NOT =   "1"   )
               IF     (SPA-HKTSANTEI-FLG   =   1     )  OR
                      (SPA-HKTSANTEI-08    =   1     )  OR
                      (SPA-NYUHKTCHK       =   1     )
                   PERFORM 5109-HKTSANTEI-CHK-SEC
      *H29.1
      *            投薬調剤料の翌月チェック
                   IF     (SPA-KIDCD           =   SPACE )
                      AND (SPA-K02N-TAIINYMD   =   "99999999")
                      AND (SPA-NYU-ERR-2       =   1     )
                       PERFORM 51092-NEXTCHOZAI-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *    包括のクリア
           IF     (SPA-HKNCOMBINUM     =   "9999")  OR
                  (SPA-CHIKENKBN       =   "1"   )
               PERFORM 51091-HOUKATU-INIT-SEC
           END-IF
      *!!!
      *    ７０歳到達の警告
      *    IF      SPA-ERRCD       =   SPACE
      *        IF      SPA-NAI-ROUFLG      =   1
      *            IF      SPA-NAI-ROUSTR      =   ZERO
      *                MOVE    "K008"          TO  SPA-ERRCD
      *            END-IF
      *        END-IF
      ***  END-IF
      *
           .
        510-TBLHEN-EXT.
           EXIT.
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       5109-HKTSANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           INITIALIZE                  ORCSC00AREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "6"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO   LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCHKTCHK-HKTFLG  =   1
               MOVE    1               TO  SPA-HKTSANTEI-CHK
           ELSE
               MOVE    ZERO            TO  SPA-HKTSANTEI-CHK
           END-IF
           IF      ORCSCHKTCHK-HKTERR  =   1
               MOVE    "0150"          TO  SPA-KIDCD
           END-IF
           .
       5109-HKTSANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    包括区分クリア処理
      *****************************************************************
       51091-HOUKATU-INIT-SEC         SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ZAI (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ERR (IDX-Z)
           END-PERFORM
           .
       51091-HOUKATU-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療エラー削除処理
      *****************************************************************
       51091-HKTSANTEI-DEL-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "3"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO   LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診療行為、計計算処理、編集
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           .
       51091-HKTSANTEI-DEL-EXT.
           EXIT.
      *
      *H29.1
      *****************************************************************
      *    投薬調剤料の翌月チェック処理
      *****************************************************************
       51092-NEXTCHOZAI-CHK-SEC             SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )
                          OR ((SPA-GMN-INPUTCD(IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE))
                          OR  (SPA-ERRCD        NOT =   SPACE )
      *
      *        薬評・減点・持参薬対象外
               IF     (SPA-COM-SRYKBN   (IDX)  =   "21"  )
                AND   (SPA-COM-YAKUHYO  (IDX)  =   SPACE  )
                AND   (SPA-COM-GENTEN   (IDX)  =   SPACE  )
                AND   (SPA-COM-JISANYAKU(IDX)  =   SPACE  )
                AND   (SPA-COM-HOUKATU-ZAI(IDX)    =   1  )
                AND   (SPA-COM-SRYSYUKBN (IDX) NOT =   "214" )
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       PERFORM 51921-DAYCHEK-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       51092-NEXTCHOZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51921-DAYCHEK-SEC             SECTION.
      *
      *
           MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
           MOVE    ZERO                TO  FLG-NEXTTUKI
      *    投薬で入院回数の時、回数集計
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )
                           OR (FLG-NEXTTUKI    =   1 )
               IF      SPA-COM-NYUINDAY (IDX IDN1) >   ZERO
                   MOVE    IDN1                TO  IDN4
                   PERFORM VARYING     IDN3    FROM    1   BY  1
                           UNTIL      (IDN3    >   WRK-KAISU )
                                   OR (FLG-NEXTTUKI    =   1 )
      *                翌月あり
                       IF      IDN4            >   SPA-SRYLST-DD
                           MOVE    1               TO  FLG-NEXTTUKI
                       END-IF
                       ADD     1                   TO  IDN4
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF      FLG-NEXTTUKI        =   1
               IF      SPA-COM-KZMFLGN2 (IDX)  =   SPACE
                   MOVE    "K190"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR  (IDX)
                   MOVE    "1"             TO  SPA-COM-KZMFLGN2(IDX)
               END-IF
           END-IF
           .
       51921-DAYCHEK-EXT.
           EXIT.
      *H29.1
      *H29.1
      *
      *H30.9
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       510112-SELCOM-CHK-SEC       SECTION.
      *
           COMPUTE IDX-STR             =   SPA-GMN-IDX  -  1
      *    直前に＊があれば、エラー
           IF     (SPA-COM-ZAIEND (IDX-STR)    =   "1" )
              AND (SPA-COM-KAIFLG (IDX-STR)    =   "1" )
               GO      TO      510112-SELCOM-CHK-EXT
           END-IF
      *
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
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       51011-SELCOM-K98-SEC       SECTION.
      *
           MOVE    SPA-SELCOM-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-K98-AREA
           MOVE    "C"                 TO  SPA-K98-GMN-SYORI
           MOVE    "//S"               TO  SPA-K98-INPUTCD
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-SELCOM-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-K98-SYORI-SEC
           .
       51011-SELCOM-K98-EXT.
           EXIT.
      *TEST
      *TEST
      *!!!!
      *
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51011-KAISU-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
               GO      TO      51011-KAISU-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-KAISU
           PERFORM VARYING     IDX-KAI FROM    1   BY  1
                   UNTIL       IDX-KAI     >   CONS-INPUT
               IF      FLG-OK              =   1
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1) NUMERIC
                       MOVE    1               TO  FLG-KAISU
                   END-IF
               ELSE
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1)   =   "*"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *    ＊ 以降に入力のない時、＊を削除
           IF      FLG-KAISU           =   ZERO
               INSPECT SPA-GMN-INPUTCD(IDX)
                       REPLACING   FIRST "*"  BY  " "
           END-IF
      *    訂正の時、剤の最後が回数入力となっている
           IF      SPA-COM-KAIFLG (IDX)    =   "1"
               MOVE    1               TO  FLG-KAISU
           END-IF
      *
           .
       51011-KAISU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  >   ZERO
               INITIALIZE                  ORCSCKNKCHKAREA
               MOVE    "2"                 TO  ORCSCKNKCHK-KBN 
               CALL    "ORCSCKNKCHK"       USING
                                           ORCSCKNKCHKAREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               IF      SPA-K021-MAX        >   ZERO
                   MOVE    1                   TO  FLG-KINKI
                   MOVE    1                   TO  FLG-ALLFLG
                   PERFORM 51020-K021-SYORI-SEC
               END-IF
           END-IF
           .
       51010-KNKCHK-EXT.
           EXIT.
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
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
      *    IF      SPA-GMN-MEISYO-G (SPA-GMN-IDX)
      *                                =   SPACE
      *        GO      TO      40113-MEISYOU-CHK-EXT
      *    END-IF
      *
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)   =  "810000001" )
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)   =  "008500000" )
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)   =  "008600000" )
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "83" OR
                                                         "84")  OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0083" OR
                                                         "0084")
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0085")
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0086")
      *R02.4
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "85")
               IF     (LNK-SC97-NYURYOKU   =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )  AND
                     ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           = "83"   ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           = "0083"  )
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           = "0085"  )
                  OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           = "0086"  ))
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
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "85") OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  =  "0083"
                                                       OR "0084"
                                                       OR "0085"
                                                       OR "0086")) AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX) NOT =   SPACE)
               CONTINUE
           ELSE
               GO      TO      40113-MEISYOU-CHK-EXT
           END-IF
      *
      *    半角の空白を全角に置きかえる
           PERFORM 401131-MEISYO-HENKAN-SEC
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
      *    フリーコメントは１桁目から
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)   =   "810000001"
                                                   OR "008500000"
                                                   OR "008600000"
               MOVE    1                   TO  IDX-STR
      *R02.4   MOVE    80                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
           ELSE
               MOVE    3                   TO  IDX-STR
      *R02.4   MOVE    78                  TO  WRK-LEN
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
                   MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
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
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
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
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
      *    挿入行のチェック
           IF     WRK-INSCNT           >   1
               IF      (SPA-GMN-MAX + (WRK-INSCNT - 1))  >= SPA-MAX-LINE
                   MOVE    "0007"              TO  SPA-ERRCD
                   GO      TO      41014-GYOINSN-EXT
               END-IF
           END-IF
      *
      *
           IF      SPA-GMN-IDX         >=  SPA-MAX-LINE
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
      *!!!!!!!!!!!!!!!!!!
      *            複数行挿入
                   IF     WRK-INSCNT   >   1
                       COMPUTE IDX         =    IDX
                                           +   (WRK-INSCNT  -  1)
                   END-IF
      *!!!!!!!!!!!!!!!!!!
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
      *****IF      (SPA-GMN-IDX        =   1   )  OR
      *            (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
      *                                =   "1" )
      *        CONTINUE
      *    ELSE
      *        MOVE    "0023"              TO  SPA-ERRCD
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      41015-ZAIDEL-EXT
      **** END-IF
      *
      *    ’−’は剤内のどの位置でも削除とする
      *    対象より、前
           IF      (SPA-GMN-IDX        =   1   )  OR
                   (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
                                       =   "1" )
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  -1
                       UNTIL       IDX     <   1
                   IF      IDX             <   SPA-GMN-IDX
                       IF      SPA-COM-ZAIEND(IDX)     =   "1"
                           MOVE    1               TO  IDX
                       ELSE
                           MOVE    SPACE   TO  SPA-GMN-INPUTCD(IDX)
      *                    診療区分行まで
                           IF      SPA-GMN-SRYKBN(IDX)
                                               NOT =   SPACE
                               MOVE    1       TO  IDX
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    後ろ
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(IDX)(1:1)   =   "S") OR
                     ((IDX                     <   SPA-MAX-LINE )  AND
                      (SPA-COM-SET  (IDX + 1)  =   "1"))
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   PERFORM 410151-SETDEL2-SEC
                   MOVE    SPA-MAX-LINE    TO  IDX
               ELSE
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   IF    ((IDX                 <   SPA-MAX-LINE ) AND
                          (SPA-GMN-SRYKBN (IDX + 1)
                                               NOT =   SPACE) ) OR
                         (SPA-COM-ZAIEND(IDX)  =   "1"    )
                       MOVE    SPA-MAX-LINE             TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
      *            UNTIL      (IDX     >   SPA-MAX-LINE )  OR
      *                       (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
      *        IF      (IDX            <   SPA-MAX-LINE    ) AND
      *                (SPA-GMN-SRYKBN (IDX + 1)    NOT =   SPACE )
      *            MOVE    SPA-MAX-LINE             TO  IDX
      *        END-IF
      *    END-PERFORM
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *****************************************************************
      *    セット削除処理（剤削除）
      *****************************************************************
       410151-SETDEL2-SEC              SECTION.
      *
           COMPUTE IDX2                =   IDX     +   1
      *
           PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                   UNTIL      (IDX3    >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX3)  =   SPACE)
               IF      SPA-COM-SET     (IDX3)   =   "1"
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX3)
                   ADD     1               TO  SPA-GMN-IDX
               ELSE
                   MOVE    SPA-MAX-LINE    TO  IDX3
               END-IF
           END-PERFORM
      *
           .
       410151-SETDEL2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX   +   1
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    SPA-MAX-LINE                 TO  IDX
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
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    SPACE               TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  FLG-DELFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   IF      WRK-COM-TIMEFLG (IDX)   NOT =   ZERO
                       MOVE    1               TO  FLG-DELFLG
                       IF      WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                           "12"
                            MOVE    2               TO  FLG-DELFLG
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    NOT =   "1" )  AND
                          (WRK-COM-DATAKBN(IDX)        =   "1"  OR
                           WRK-COM-INPUTCD(IDX)        =   SPACE)
                       MOVE    1               TO  FLG-ZAIEND
                   END-IF
      *            器材商品名
                   IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "058")
                       MOVE    2               TO  FLG-ZAIEND
                   END-IF
      *            前も空白の時対象行のみ
                   IF     (WRK-COM-INPUTCD(IDX)        =   SPACE )  AND
                          (WRK-MAE-INPUTCD(IDX)        =   SPACE )
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *            前が器材商品名
                   IF     (WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE )
                      AND (WRK-COM-INPUTCD(IDX - 1)(1:3)
                                                       =   "058")
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX - 1)
                   END-IF
      *
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                     <   SPA-MAX-LINE) AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *!!!!!!!!!!!!!!!!!!
      *連続挿入解除
                   IF     (IDX                 =  SPA-SYOKAI-IDX )
                      OR  (WRK-COM-IN (IDX)    =   "1"   )
                       MOVE    SPACE           TO  SPA-SYOKAI-SRYCD
                       MOVE    SPACE           TO  SPA-SYOKAI-YMD
                       MOVE    ZERO            TO  SPA-SYOKAI-IDX
                   END-IF
      *!!!!!!!!!!!!!!!!!!
               ELSE
                   IF      FLG-ZAIEND          =   1
                       IF      WRK-COM-AUTOKBN(IDX)    NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                       END-IF
                   END-IF
      *            前が器材商品名
                   IF      FLG-ZAIEND          =   2
                       IF      WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                           MOVE    ZERO            TO  FLG-ZAIEND
                       END-IF
                   END-IF
      *            診療の時間加算削除の時、自動追加した時間加算を削除
                   IF      FLG-DELFLG          =   2
                       IF     (WRK-COM-AUTOKBN(IDX)    NOT =   SPACE)
                         AND  (WRK-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                (IDX)
                       END-IF
                   END-IF
                   IF      WRK-COM-ZAIEND (IDX)    =   "1"
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               END-IF
           END-PERFORM
      *R01.7
      **** IF      FLG-DELFLG      NOT =   ZERO
      *        MOVE    ZERO                TO  SPA-NAI-TIMEFLG
      **** END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
      *            登録前チェックの時、診療種別のみの剤は削除する
                   IF      FLG-TOROKU          =   "1"
                       IF      WRK-GMN-INPUTCD (IDX)(1:1)  =  "."
                           IF      (IDX            =   SPA-MAX-LINE ) OR
                                   (WRK-GMN-INPUTCD(IDX + 1)(1:1)
                                                   =   "."     )  OR
                                   (WRK-COM-INPUTCD(IDX + 1)
                                                   =   SPACE  AND
                                    WRK-GMN-INPUTCD(IDX + 1)
                                                   =   SPACE   )
      *??H24.11                OR  (WRK-GMN-SRYKBN (IDX + 1)
      ****                                         NOT =   SPACE )
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      FLG-ERR             =   ZERO
      *
                       ADD     1               TO  SPA-GMN-MAX
                       MOVE    WRK-COM-TBLREC (IDX) 
                                               TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                       MOVE    WRK-GMN-TBLREC (IDX) 
                                               TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
                       IF      SPA-GMN-MAX     NOT =   IDX
                           MOVE    "1"             TO  FLG-SYORI
                       END-IF
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択処理
      *****************************************************************
       41014-DOSELNUM-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    K02N-DOSELNUM       TO  SPA-GMN-DOSELNUM
           IF      K02N-DOSELNUM       =   SPACE
               MOVE    ZERO            TO  SPA-NAI-SELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    K02N-DOSELNUM       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "1000"              TO  SPA-ERRCD
                   GO      TO      41014-DOSELNUM-EXT
               ELSE
                   MOVE    SNUM-OUTNUM       TO  SPA-NAI-SELNUM
               END-IF
           END-IF
    *
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
              IF      SPA-GMN-NO (IDX)     =   SPA-NAI-SELNUM
                  MOVE    IDX                  TO  SPA-NAI-DOSELNUM
                  MOVE    SPA-GMN-MAX1         TO  IDX
              END-IF
           END-PERFORM
      *
      *D   MOVE    SPA-NAI-DOSELNUM    TO  WRK-ZZ
           MOVE    SPA-NAI-SELNUM      TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
           IF      SPA-NAI-DOSELNUM    <   01   OR
                                       >   SPA-GMN-MAX1
               MOVE    "1000"              TO  SPA-ERRCD
           ELSE
      *
               IF      SPA-NAI-SYORI       =   ZERO
      *            ＤＯ検索処理へ
                   PERFORM 4801-K09-SYORI-SEC
               ELSE
      *            訂正日編集処理
                   PERFORM 410141-TEISEI-SET-SEC
               END-IF
           END-IF
      *
           .
       41014-DOSELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       41015-RRKLIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    ZERO                TO  WRK-DOSELNUM
           MOVE    ZERO                TO  WRK-DOSELNUM2
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX1
              IF      K02N-SELECT (IDX)        =  "T"
                  IF      IDX                  =   SPA-NAI-TESELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-NO (IDX)    TO  WRK-DOSELNUM2
                      MOVE    IDX                 TO  WRK-SELNUM2
                  END-IF
                  MOVE    SPA-GMN-NO (IDX)    TO  WRK-DOSELNUM
                  MOVE    IDX                 TO  WRK-SELNUM
              END-IF
           END-PERFORM
      *    番号
           IF      WRK-DOSELNUM2       >   ZERO
      *****    MOVE    WRK-DOSELNUM2       TO  SPA-NAI-DOSELNUM
               MOVE    WRK-DOSELNUM        TO  SPA-NAI-SELNUM
           ELSE
      ****     MOVE    WRK-DOSELNUM        TO  SPA-NAI-DOSELNUM
               MOVE    WRK-DOSELNUM        TO  SPA-NAI-SELNUM
           END-IF
      *    テーブル位置
           IF      WRK-SELNUM2        >   ZERO
               MOVE    WRK-SELNUM2        TO  SPA-NAI-DOSELNUM
           ELSE
               MOVE    WRK-SELNUM         TO  SPA-NAI-DOSELNUM
           END-IF
      *
           IF      SPA-NAI-DOSELNUM    <   01   OR
                                       >   SPA-GMN-MAX1
               CONTINUE
           ELSE
               MOVE    K02N-NO  (SPA-NAI-DOSELNUM)
                                           TO  SPA-GMN-DOSELNUM
      *
               IF      SPA-NAI-SYORI       =   ZERO
      *            ＤＯ検索処理へ
                   PERFORM 4801-K09-SYORI-SEC
               ELSE
      *            訂正日編集処理
                   PERFORM 410141-TEISEI-SET-SEC
               END-IF
           END-IF
      *
      *
           .
       41015-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正日編集処理
      *****************************************************************
       410141-TEISEI-SET-SEC           SECTION.
      *
      *H25.2
      *    レセ電移行分はエラー
           IF      SPA-NAI-RRK-SRYKA (SPA-NAI-DOSELNUM)(1:1)
                                       =   "A"
               MOVE    "7011"              TO  SPA-ERRCD
               GO      TO      410141-TEISEI-SET-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    診療日付
           MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYYMDW
      *    労災・自賠責の時、日付のみ
           IF      SPA-NAI-RRK-HKNKBN(SPA-NAI-DOSELNUM)  =   "1"
                                                         OR  "2"
                                                         OR  "4"
                                                         OR  "5"
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)(1:12)
                                           TO  SPA-SRYYMDW
           END-IF
      *
           MOVE    SPA-NAI-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYYMD
           MOVE    SPA-NAI-RENNUM (SPA-NAI-DOSELNUM)
                                       TO  SPA-RENNUM
           MOVE    SPA-NAI-DOUJI-RENNUM (SPA-NAI-DOSELNUM)
                                       TO  SPA-DOUJI-RENNUM
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
      *    訂正処理
           MOVE    1                   TO  SPA-SYORIFLG
      *!!
           IF      SPA-RENNUM          =   2
               MOVE   1                    TO  FLG-DOUNYUIN
           ELSE
               MOVE   ZERO                 TO  FLG-DOUNYUIN
           END-IF
      *    入院が存在するかのチェック
           PERFORM 410101-NYUINRRK-CHK-SEC
           IF      SPA-NYUGAIKBN       =   "2"
      *        入院期間外
               MOVE    "1"                 TO  SPA-NYUGAIKBN
               MOVE    "9032"              TO  WRK-ERRCD
               MOVE    "9032"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-KIDCD
           ELSE
               MOVE    SPACE               TO  WRK-ERRCD
           END-IF
      *!!!
           IF     (SPA-RENNUM      NOT =   1  )
             AND  (SPA-K02N-DOUDAY     =   ZERO)
               MOVE    "0171"              TO  WRK-ERRCD
               MOVE    2                   TO  SPA-NYU-ERR
           END-IF
      *
      *    診療科
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYKA
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-SRYKA
      *    保険組合せ
           MOVE    SPA-NAI-RRK-HKNCOMBI(SPA-NAI-DOSELNUM)
                                       TO  SPA-HKNCOMBINUM
           MOVE    SPA-NAI-RRK-HKNCOMBI(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-HKNCOMBI
      *
      *
      *    ＳＰＡ編集処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  ORCSC95AREA
           MOVE    "4"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           MOVE    SPA-SRYYMD          TO  LNK-SC95-RRKYMD
           MOVE    SPA-RENNUM          TO  LNK-SC95-RENNUM
           MOVE    SPA-DOUJI-RENNUM    TO  LNK-SC95I-DOUJI-RENNUM
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           IF      SPA-HKNCOMBINUM NOT =   SPA-OLD-HKNCOMBI
               MOVE    1               TO  SPA-HKN-ERR2
           END-IF
      *
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    訂正処理
      *****MOVE    1                   TO  SPA-SYORIFLG
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
           MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-GMN-SRYYMD
      *    労災・自賠責の時、日付のみ
           IF      SPA-NAI-RRK-HKNKBN(SPA-NAI-DOSELNUM)  =   "1"
                                                         OR  "2"
                                                         OR  "4"
                                                         OR  "5"
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)(1:12)
                                           TO  SPA-GMN-SRYYMD
           END-IF
      *
           INITIALIZE                      SPA-NAIACCT2-AREA
           INITIALIZE                      SPA-RIGAKU-NCNT-AREA
      *    当月診療行為チェック用編集、当月累計点数計算
      *****PERFORM 3201-TOUCHKSET-SEC
      *    点滴の変更
      *    IF      LNK-SC95-TENTEKI    =   1
      *        MOVE    SPACE           TO  SPA-ACCT2-SRYCD
      *    END-IF
      *    COMPUTE SPA-ACCT2-CSYYOURYO
      *                                =   SPA-ACCT2-CSYYOURYO
      *********************************-   LNK-SC95-CHUSYA
      *
      *    画面用スパ編集処理
           MOVE    1                   TO  FLG-TEISEI
           PERFORM 310-SPA-GMNSET-SEC
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
           END-IF
      *
      *    ＤＯ編集へ戻す
           MOVE    "F"                 TO  SPA-GMN-TEISEI
           MOVE    ZERO                TO  SPA-NAI-SYORI
           MOVE    SPA-NAI-DOSELNUM    TO  SPA-NAI-TESELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    SPACE               TO  SPA-GMN-DOSELNUM
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *??  MOVE    1                   TO  SPA-GMN-CUR
      *    １行目へ
           MOVE    98                  TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-MAP-IDX
      *
           .
       410141-TEISEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    MOVE    ZERO                TO  SPA-RJN-ERR
      *    MOVE    ZERO                TO  SPA-HKN-ERR
      *    MOVE    ZERO                TO  SPA-NYU-ERR
      *    MOVE    ZERO                TO  SPA-NAI-KOHFLG-ERR
      *    MOVE    ZERO                TO  SPA-NAI-KOHFLG
      *    MOVE    ZERO                TO  SPA-NAI-ZENKIFLG-ERR
      **** MOVE    ZERO                TO  SPA-NAI-ZENKIFLG
           IF      SPA-MACHIPG         =   "U02"
                                       OR  "Q02"
                                       OR  "Q02A"
                                       OR  "Q02B"
     *******   MOVE    "U02"               TO  WRK-SAKIPG
               MOVE    SPA-MACHIPG         TO  WRK-SAKIPG
               PERFORM 210-BACK-SYORI-SEC
           ELSE
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    "M01"               TO  WRK-SAKIPG
                   PERFORM 210-BACK-SYORI-SEC
               ELSE
      *            ガイドメッセージ表示へ
                   MOVE    "0104"              TO  SPA-KIDCD
                   PERFORM 520-KIDSET-SEC
               END-IF
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC               SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    WRK-SAKIPG          TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻るのクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME2          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME22         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      ****                             ORCSFDELAREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           MOVE    1                   TO  FLG-END2
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理（確定）
      *****************************************************************
       210-BACK-OK-SEC                SECTION.
      *
           PERFORM 210-PARA-DEL-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       222-PTNUMCLEAR-SEC             SECTION.
      *
      *     クリア確認チェック
           IF      (SPA-CLEARCHK-FLG   =   "1"   )  AND
                   (SPA-GMN-PTID   NOT =   ZERO  )
               AND (SPA-GMN-MAX        >   ZERO  )
               MOVE    "0125"              TO  SPA-KIDCD
               PERFORM 520-KIDSET-SEC
           ELSE
               PERFORM 2221-PTNUMCLEAR-SYORI-SEC
           END-IF
      *
           .
       222-PTNUMCLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       2221-PTNUMCLEAR-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
      *    一時データ削除
           PERFORM 210-PARA-DEL-SEC
           MOVE    ZERO                TO  FLG-END2
      *
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-SCR-REC
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    0               TO  SPA-GMN-CUR
      *
           .
       2221-PTNUMCLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
      *    確認メッセージ
           IF      SPA-GMN-PTID    NOT =   ZERO
               MOVE    "0116"              TO  SPA-KIDCD
           END-IF
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SYORI-SEC            SECTION.
      *
      *****IF      SPA-GMN-PTID    NOT =   ZERO
      *        ワーク診療行為クリア
      *        PERFORM 4900-WKSRYACT-CLEAR-SEC
      *****END-IF
      *
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    SPA-K02N-JIKANKBN   TO  SPA-NAI-TIMEFLG
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           IF      SPA-SYORIFLG        =   ZERO
      *        排他中は削除なし
             AND  (SPA-LOCK            =   ZERO )
               PERFORM 2201-WKSRYACT-CHK-SEC
           ELSE
               MOVE    ZERO            TO  FLG-WKSRYACT-ARI
           END-IF
           IF      FLG-WKSRYACT-ARI    =   ZERO
               CONTINUE
           ELSE
               MOVE    "0127"              TO  SPA-KIDCD
           END-IF
      *
           .
       220-CLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了展開中の判定処理
      *****************************************************************
       2201-WKSRYACT-CHK-SEC            SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  FLG-WKSRYACT-ARI
           PERFORM
               UNTIL  (FLG-WKSRYACT        =   1   )  OR
                      (FLG-WKSRYACT-ARI    =   1   )
               IF      WKSRY-MOD-FLG   NOT =   ZERO
                   MOVE    1               TO  FLG-WKSRYACT-ARI
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2201-WKSRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了展開中の削除処理
      *****************************************************************
       2202-CLEAR-WKSRYACT-SEC            SECTION.
      *
      *    展開中の削除
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               IF      WKSRY-MOD-FLG       NOT =   ZERO
                   MOVE    WKSRYACT-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1999"          TO  SPA-ERRCD
                       DISPLAY "K02 WKSRYACT DEL MCP-RC:" MCP-RC
                               ",KEY:" WKSRY-KEY
                   END-IF
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2202-CLEAR-WKSRYACT-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為クリア処理
      *****************************************************************
       4900-WKSRYACT-CLEAR-SEC             SECTION.
      *
           IF      SPA-LOCK            =   ZERO
               CONTINUE
           ELSE
               GO      TO      4900-WKSRYACT-CLEAR-EXT
           END-IF
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
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               EVALUATE    FLG-MOD-FLG
                   WHEN    1
      *                展開中止
                       IF      WKSRY-MOD-FLG       =   9
                           MOVE    ZERO                TO  WKSRY-MOD-FLG
                       END-IF
                   WHEN    2
      *                前の展開
                       IF      WKSRY-MOD-FLG       =   9
                           MOVE    1                   TO  WKSRY-MOD-FLG
                       ELSE
                           IF      WKSRY-MOD-FLG       =   1
                               MOVE    ZERO            TO  WKSRY-MOD-FLG
                           END-IF
                       END-IF
                   WHEN    0
                       MOVE    ZERO                TO  WKSRY-MOD-FLG
               END-EVALUATE
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1999"          TO  SPA-ERRCD
                   DISPLAY "K02 WKSRYACT UPD MCP-RC:" MCP-RC
                               ",KEY:" WKSRY-KEY
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4900-WKSRYACT-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録画面遷移処理
      *****************************************************************
       420-KOUISET-SEC             SECTION.
      *
      *****患者存在チェック
      *    IF      SPA-PTID            =   ZERO
      *        MOVE    "1005"              TO  SPA-ERRCD
      *        MOVE    00                  TO  SPA-GMN-CUR
      *        GO      TO      420-KOUISET-EXT
      *    END-IF
      *    IF      SPA-HKNCOMBINUM     =   SPACE
      *        MOVE    "1006"          TO  SPA-ERRCD
      *        MOVE    00                  TO  SPA-GMN-CUR
      *        GO      TO      420-KOUISET-EXT
      *****END-IF
      *    患者が確定してない時、戻った時表示を空白
           IF      SPA-PTID            =   ZERO
               MOVE    SPACE           TO  SPA-GMN-PTNUM
           ELSE
      *        診療行為入力チェック
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      420-KOUISET-EXT
               END-IF
      *
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K05"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       420-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新画面　処理
      *****************************************************************
       430-SANTEIRRK-SEC             SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               MOVE    3                   TO  SPA-HKN-ERR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K06"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-SANTEIRRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード設定登録　処理
      *****************************************************************
       4106S-INPUTCD-SYORI-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
      *    初期表示
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK023"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           IF      SPA-K023-MAX        =   ZERO
               MOVE    "1012"              TO  SPA-ERRCD
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *    画面カーソルセット処理
           MOVE    1                   TO  SPA-K023-CUR
           MOVE    1                   TO  SPA-K023-PAGE
           MOVE    1                   TO  SPA-K023-IDX
           MOVE    1                   TO  SPA-K023-MAP
           MOVE    "INPUTCD101"        TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K023"              TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K023"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4106S-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録　処理
      *****************************************************************
       440-BYOTOROKU-SEC             SECTION.
      *
           IF      SPA-GMN-PTID      =   ZERO
      *        病名登録遷移処理
               PERFORM 4401-C02SET-SEC
               GO      TO      440-BYOTOROKU-EXT
           END-IF
      *
      *    基本チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1      )
               CONTINUE
           ELSE
      *        病名登録遷移処理
               PERFORM 4401-C02SET-SEC
           END-IF
      *
      *!   更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      440-BYOTOROKU-EXT
      *!   END-IF
      *
      *    禁忌更新前のチェック
      *    PERFORM 41001-YAKKNK-CHK-SEC
      *
      *     禁忌エラーの時、エラー表示へ
      *    IF      WRK-KNKCHK-AREA     NOT =   SPACE
      *        MOVE    "C02"               TO  SPA-K021-SAKI
      *        PERFORM 410019-POP-K021-SEC
      *        GO      TO      440-BYOTOROKU-EXT
      *    END-IF
      *    病名登録遷移処理
      *!!  PERFORM 4401-C02SET-SEC
           .
       440-BYOTOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録遷移処理
      *****************************************************************
       4401-C02SET-SEC             SECTION.
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "C02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4401-C02SET-EXT.
           EXIT.
      *    
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       440-KNJADD-SEC             SECTION.
      *
      *
           IF      SPA-PTNUM(1:1)      =   "*"
               MOVE    ZERO                TO  SPA-PTID
           ELSE
      *
      *        患者存在チェック
               IF      SPA-PTID            =   ZERO
                   MOVE    SPACE               TO  SPA-PTNUM
               ELSE
      *
      *            診療行為入力チェック
                   MOVE    1                   TO  FLG-HENKOU
                   PERFORM 41011-KOUI-ALLHEN-SEC
                   IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                          (SPA-KIDCD           =   "0180" )  OR
                          (FLG-END             =   1      )
                      GO      TO      440-KNJADD-EXT
                   END-IF
      *********更新前のチェック
      *            MOVE    "1"                 TO  FLG-TOROKU
      *            PERFORM 510-TBLHEN-SEC
      *            MOVE    SPACE               TO  FLG-TOROKU
      *            IF      SPA-ERRCD       NOT =   SPACE
      *               GO      TO      440-KNJADD-EXT
      ***********  END-IF
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       440-KNJADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計カードへ
      *****************************************************************
       450-KAIKEI-SEC             SECTION.
      *
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      450-KAIKEI-EXT
               END-IF
      *
      ******** 更新前のチェック
      *        MOVE    "1"                 TO  FLG-TOROKU
      *        PERFORM 510-TBLHEN-SEC
      *        MOVE    SPACE               TO  FLG-TOROKU
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      450-KAIKEI-EXT
      ******** END-IF
      *
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "J02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       450-KAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録へ
      *****************************************************************
       460-SYUNOU-SEC             SECTION.
      *
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      460-SYUNOU-EXT
               END-IF
      *
      ******** 更新前のチェック
      *        MOVE    "1"                 TO  FLG-TOROKU
      *        PERFORM 510-TBLHEN-SEC
      *        MOVE    SPACE               TO  FLG-TOROKU
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      460-SYUNOU-EXT
      ******** END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "S02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       460-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院登録へ
      *****************************************************************
       4106S-NYUTAIIN-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      4106S-NYUTAIIN-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "I01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
      *?   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *?   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "I01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       4106S-NYUTAIIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院登録へ
      *****************************************************************
       4111S-NYUKAIKEI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      4111S-NYUKAIKEI-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "I41"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
      *?   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *?   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "I41"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       4111S-NYUKAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *   ワーク行為負担パラメタを作成　処理
      *****************************************************************
       4410-WSINPARA-SEC             SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "4"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  FLG-END2
           END-IF
      *
           .
       4410-WSINPARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    途中終了　処理
      *****************************************************************
       4110-KYOSEIEND-SEC             SECTION.
      *
      *    訂正中の時、中途終了できない。
           IF      SPA-SYORIFLG        =   1
               MOVE    "1024"          TO  SPA-ERRCD
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
      *H26.12
           IF      SPA-ERRCD           =   "0178"
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-KIDCD
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
           PERFORM 45109-SNRYO-NYU-SEC
      *
           .
       4110-KYOSEIEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
      *H25.5  警告メッセージ表示対応
      **** MOVE    1                  TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      450-MAEGMN-EXT
           END-IF
      *
      *v4.7
      *    行位置とカーソル位置の決定
           PERFORM 10021-K02SPA-INIT-SET-SEC
      *
           IF      K02N-PANDATABLE1-TROW   =  ZERO
               MOVE    ZERO                TO  WRK-IDX2
           ELSE
               MOVE    K02N-PANDATABLE1-TROW    TO  WRK-IDX2
               IF      K02N-INPUTCD(WRK-IDX2)   =   SPACE
                   MOVE    ZERO                TO  WRK-IDX2
               END-IF
           END-IF
      *!!
      *画面の位置取得できないのでゼロ設定
      *!!!
      **** MOVE    ZERO                TO  WRK-IDX2
      *!!
           IF      WRK-IDX2            =   ZERO
               MOVE    SPA-MAP-IDX     TO  WRK-IDX2
           END-IF
           IF     (WRK-IDX2            >   WRK-MAP-MAX )
               MOVE    WRK-MAP-MAX         TO  WRK-IDX2
           END-IF
           IF      WRK-IDX2            >   SPA-GMLINCNT
               COMPUTE WRK-GYO         =   WRK-IDX2    -   SPA-GMLINCNT
           ELSE
               MOVE    1               TO  WRK-GYO
           END-IF
      *
           PERFORM VARYING     IDX     FROM    WRK-IDX2  BY  -1
                   UNTIL      (IDX     <   1   )
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-GYO      )
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                   MOVE    1                   TO  IDX
                   MOVE    1                   TO  FLG-F6
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
           IF      FLG-F6              =   ZERO
               MOVE    "1"                 TO  SPA-COM-CUR (1)
           END-IF
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
      *H25.5  警告メッセージ表示対応
      **** MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180")  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
      *v4.7
      *    行位置とカーソル位置の決定
           PERFORM 10021-K02SPA-INIT-SET-SEC
      *
           IF     (K02N-PANDATABLE1-TROW   =  ZERO )
               MOVE    ZERO                TO  WRK-IDX2
           ELSE
               MOVE    K02N-PANDATABLE1-TROW   TO  WRK-IDX2
               IF      K02N-INPUTCD(WRK-IDX2)  =   SPACE
                   MOVE    ZERO                TO  WRK-IDX2
               END-IF
           END-IF
      *!!! カーソル位置不明
      **** MOVE    ZERO                TO  WRK-IDX2
      *!!!
           IF      WRK-IDX2            =   ZERO
               MOVE    SPA-MAP-IDX     TO  WRK-IDX2
           END-IF
           COMPUTE WRK-GYO             =   WRK-IDX2 +   SPA-GMLINCNT
      *
           MOVE    ZERO                TO  FLG-F7
           PERFORM VARYING     IDX     FROM    WRK-IDX2  BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )
                           OR (IDX     >   SPA-GMN-MAX    )
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-GYO      )
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                   MOVE    SPA-MAX-LINE        TO  IDX
                   MOVE    1                   TO  FLG-F7
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行目へ　処理
      *****************************************************************
       4501-CTRHOME-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE  )  OR
                  (FLG-END             =   1     )
               GO      TO      4501-CTRHOME-EXT
           END-IF
      *
           MOVE    01                  TO  SPA-GMN-CUR
           MOVE    "1"                 TO  SPA-COM-CUR (1)
           .
       4501-CTRHOME-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴 前頁　処理
      *****************************************************************
       4109-BC06-SYORI-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    "7008"              TO  SPA-ERRCD
           ELSE
               IF      SPA-RRK-PAGE        <=  1
                   MOVE    "7010"              TO  SPA-ERRCD
               ELSE
                   COMPUTE SPA-RRK-PAGE    =   SPA-RRK-PAGE  -  1
      *
                   INITIALIZE                  ORCSC95AREA
                   MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
                   MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
                   MOVE    "N"                 TO  LNK-SC95-KBN
                   MOVE    "K02"               TO  LNK-SC95-PG
                   CALL    "ORCSC95"           USING
                                               MCPAREA
                                               ORCSC95AREA
                                               SPA-AREA
                                               SPA-K01KYOUTU
                                               SPA-K02
                   MOVE    1                   TO  FLG-GMN-INIT
               END-IF
           END-IF
           .
       4109-BC06-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 次頁　処理
      *****************************************************************
       4109-BC07-SYORI-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    "7008"              TO  SPA-ERRCD
           END-IF
           IF      SPA-RRK-NEXT        =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "7009"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-RRK-PAGE    =   SPA-RRK-PAGE  +  1
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
               MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
               MOVE    "N"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
               MOVE    1                   TO  FLG-GMN-INIT
           END-IF
      *
           .
       4109-BC07-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前カーソル　処理
      *****************************************************************
       450-MAECUR-SEC             SECTION.
      *
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
      *            診療科へ
                   MOVE    06              TO  SPA-GMN-CUR
               WHEN    06
                   MOVE    07              TO  SPA-GMN-CUR
               WHEN    07
                   MOVE    05              TO  SPA-GMN-CUR
               WHEN    05
                   MOVE    ZERO            TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       450-MAECUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       480-DOSYORI-SEC            SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    00              TO  SPA-GMN-CUR
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
      *H28.6
      *    数量ゼロはＤＯへ
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      **** MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    SPACE               TO  SPA-GMN-DOSELNUM
           MOVE    1                   TO  SPA-NAI-DOSELNUM
           MOVE    SPA-GMN-NO (1)      TO  SPA-NAI-SELNUM
      **** MOVE    SPA-NAI-DOSELNUM    TO  WRK-ZZ
      **** MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
      *    ＤＯ検索処理へ
           PERFORM 4801-K09-SYORI-SEC
           .
       480-DOSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       4801-K09-SYORI-SEC            SECTION.
      *
      *    行挿入行をなくす
           PERFORM 420-SYOKYO-SEC
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK09"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K09"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K09"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-K09-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０１処理へ
      *****************************************************************
       4001-K01-SYORI-SEC                   SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4001-K01-SYORI-EXT
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K01"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    "K01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4001-K01-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定処理へ
      *****************************************************************
       470-SANTEI-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      470-SANTEI-EXT
           END-IF
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           CALL    "ORCGK04"           USING
                   MCPAREA
                   SPAAREA
                   LINKAREA
                   SCRAREA.
      *
      *    初期カーソル設定
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K04"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SANTEI-EXT.
           EXIT.
      *    
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-LAST-PTNUM      TO  K02N-PTNUM
               PERFORM 41010-PTNUM-SYORI-SEC
           END-IF
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *    
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-P97-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    WRK-P97-NAME        TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-P97-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" ) OR
                  (FLG-END             =   1     )
               GO      TO      480-YOYAKU-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" ) OR
                  (FLG-END             =   1     )
               GO      TO      490-UKEICHI-EXT
           END-IF
      *
      *    更新前のチェック
      **   MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      490-UKEICHI-EXT
      *****END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "U01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付画面へ
      *****************************************************************
       4102-UKETUKE-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" ) OR
                  (FLG-END             =   1     )
               GO      TO      4102-UKETUKE-EXT
           END-IF
      *
      *****更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      4102-UKETUKE-EXT
      *****END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *
           MOVE    "U02"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPA-K02             TO  LNK-SCRSPA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "U02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4102-UKETUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬帳票印刷画面へ処理
      *****************************************************************
       4124-MAESYOHOU-SYORI-SEC             SECTION.
      *
      *    患者の確定されてない時、処理をしない
           IF     (SPA-GMN-PTNUM       =   SPACE )  OR
                  (SPA-PTID            =   ZERO  )
               GO      TO      4124-MAESYOHOU-SYORI-EXT
           END-IF
      *
      *!!!!
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      4124-MAESYOHOU-SYORI-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG3
           MOVE    "KA02"              TO  SPA-SAKIPG
      *
      *****MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "KA02"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4124-MAESYOHOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院日処理
      *****************************************************************
       4125-NYUINRRK-NEXT-SEC             SECTION.
      *
           IF     (SPA-SYORIFLG        =   ZERO )
             AND ((SPA-K02N-DOUNYUKBN  =   1    )  OR
                  (SPA-K02N-DOUKBN     =   "1"  ))
               IF      SPA-GMN-MAX         >   ZERO
                   MOVE    "0200"              TO  SPA-KIDCD
               ELSE
                   MOVE    ZERO                TO  FLG-NYUNEXT
                   PERFORM 41251-NYUINRRK-NEXTSYORI-SEC
               END-IF
           END-IF
      *
           .
       4125-NYUINRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院日処理
      *****************************************************************
       41251-NYUINRRK-NEXTSYORI-SEC    SECTION.
      *
           IF      SPA-K02N-DOUNYUKBN  =   1
               MOVE    1                   TO  FLG-DOUNYUIN
           ELSE
               MOVE    ZERO                TO  FLG-DOUNYUIN
           END-IF
      *
      *    入院期間変更
           PERFORM 410101-NYUINRRK-CHK-SEC
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    前回展開
           MOVE    ZERO                TO  FLG-MOD-FLG
      *    再表示編集処理
           IF      LNK-SC95-WKSRYACT   =   1
      *        中途終了に変更
               IF      SPA-GMN-MAX         >   ZERO
                   MOVE    ZERO            TO  FLG-NYUNEXT
                   MOVE    "XXXX"          TO  WRK-ERRCD
      *            前回展開中中止
                   MOVE    1               TO  FLG-MOD-FLG
                   PERFORM 4900-WKSRYACT-CLEAR-SEC
               END-IF
           ELSE
      *        中途終了に変更
               IF     (SPA-GMN-MAX         >   ZERO )
                 AND  (FLG-NYUNEXT         =   ZERO )
                   MOVE    ZERO            TO  FLG-MOD-FLG
                   PERFORM 4900-WKSRYACT-CLEAR-SEC
               END-IF
           END-IF
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41251-NYUINRRK-NEXTSYORI-EXT
           END-IF
      *
      *    チェック用月内診療行為セット
      *    PERFORM                 320-SPA-CHKSET-SEC
      *    中途終了データクリア
           IF      FLG-NYUNEXT         =   1
      *        前回展開
               MOVE    2                   TO  FLG-MOD-FLG
               PERFORM 4900-WKSRYACT-CLEAR-SEC
           END-IF
      ***  PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
           IF      FLG-NYUNEXT         =   1
      *        内容を残す
               GO      TO      41251-NYUINRRK-NEXTSYORI-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    初診料自動発生
           PERFORM 310-SPA-GMNSET-SEC
      *
           IF      WRK-ERRCD           =    "XXXX"
               MOVE    "0172"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           .
       41251-NYUINRRK-NEXTSYORI-EXT.
           EXIT.
      *
      *H29.5
      *****************************************************************
      *    メモ登録処理へ
      *****************************************************************
       4188-MEMO-SYORI-SEC             SECTION.
      *
      *    患者の確定されてない時、処理をしない
           IF     (SPA-GMN-PTNUM       =   SPACE )  OR
                  (SPA-PTID            =   ZERO  )
               GO      TO      4188-MEMO-SYORI-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4188-MEMO-SYORI-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "XF01"              TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "XF01"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4188-MEMO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘルプ処理
      *****************************************************************
       4199-HELP-SYORI-SEC             SECTION.
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKHELP"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    戻り先
           MOVE    "K02N"              TO  SPA-ETC-MOTOPG
           MOVE    "KHELP"             TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KHELP"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4199-HELP-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正処理
      *****************************************************************
       4101-TEISEI-SEC             SECTION.
      *
           MOVE    04                  TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-GMN-TEISEI
      *        ＤＯ検索
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    ZERO            TO  SPA-NAI-SYORI
      *        訂正診療日選択
               WHEN    "F"
      *            診療室入力の時、訂正はできないこととする
                   IF      SPA-BUNSAN          =   "1"
                       MOVE    "1026"          TO  SPA-ERRCD
                       GO      TO      4101-TEISEI-EXT
                   END-IF
                   MOVE    "T"             TO  SPA-GMN-TEISEI
                   MOVE    1               TO  SPA-NAI-SYORI
           END-EVALUATE
      *
           .
       4101-TEISEI-EXT.
           EXIT.
      *****************************************************************
      *    検査処理（トグルボタン）
      *****************************************************************
       4109-KENSA-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-K02N-KENSAFLG
      *        包括中
               WHEN    "T"
      *            包括終了
                   MOVE    "F"             TO  SPA-K02N-KENSAFLG
      *        包括なし
               WHEN    "F"
      *            包括開始
                   MOVE    "T"             TO  SPA-K02N-KENSAFLG
                   IF     (SPA-PTID    NOT =   ZERO )  AND
                          (SPA-GMN-MAX     >   ZERO )
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 510-TBLHEN-SEC
                       IF      ORCSC100-KENSAERR   =   1
                           MOVE    "1062"          TO  SPA-ERRCD
                           MOVE    "F"             TO  SPA-K02N-KENSAFLG
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       4109-KENSA-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前の基本チェック処理
      *****************************************************************
       41001-KIHON-CHK-SEC             SECTION.
      *
           IF      SPA-PTNUM           =   SPACE
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
           ELSE
      *        患者マスター存在チェック
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF     (FLG-PTINF           =   1     )  OR
                      (SPA-PTNUM           =   SPACE )
                   MOVE    "1003"              TO  SPA-ERRCD
                   MOVE    00                  TO  SPA-GMN-CUR
               ELSE
                   IF      SPA-HKNCOMBINUM     =   SPACE
                       MOVE    "1006"          TO  SPA-ERRCD
                       MOVE    3               TO  SPA-HKN-ERR
                       MOVE    00              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-RJN-ERR
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1027"              TO  SPA-ERRCD
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    2
                       MOVE    "1070"              TO  SPA-ERRCD
      *            後期高齢者の保険がない時
                   WHEN    3
                       IF      SPA-NAI-ROUSTR      =   ZERO
                           MOVE    "1076"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-NAI-ROUSTR
                       END-IF
      *            退職国保で６５歳以上
                   WHEN    9
                       MOVE    "1077"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
      *            保険未選択の場合、エラー
                   WHEN    3
                       MOVE    "1006"              TO  SPA-ERRCD
                   WHEN    4
                       MOVE    "1034"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
      *H24.9
      *            アフターケアの中途データ展開
                   WHEN    9
                       MOVE    "9041"              TO  SPA-ERRCD
               END-EVALUATE
      *H24.9
               IF      SPA-ERRCD           =   SPACE
      *            アフターケアエラー
                   IF     (SPA-HKNNUM          =   "971" ) AND
                          (SPA-RSI-HKNKBN      =   "3" )
                       MOVE    "9029"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    入院料設定ないエラー
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NYU-ERR         =   1
                   MOVE    "0217"              TO  SPA-ERRCD
                   IF      SPA-LOCK            =   ZERO
                       MOVE    20                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF     (SPA-NYU-ERR         =   2    )
                  AND (SPA-GMN-MAX         >   ZERO )
                   MOVE    "0171"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    地方公費のみの場合、警告
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-KOHFLG      =   1
                  IF      SPA-NAI-KOHFLG-ERR  =   ZERO
                       MOVE    "K034"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    国保 前期高齢者変更チェック
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-ZENKIFLG      =   1
                   IF      SPA-NAI-ZENKIFLG-ERR    =   ZERO
                       MOVE    "K171"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE  )
      *H26.12     中途終了時は行う
            AND   (SPA-KIDCD       NOT =   "0117" )
               IF      SPA-NAI-JYURRKFLG   =   1
                   MOVE    "0178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JYURRKFLG   =   2
                   MOVE    "K178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
                   MOVE    ZERO                TO  SPA-NAI-JYURRKFLG
               END-IF
           END-IF
      *
      *R02.1
      *        患者登録で変更
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-PTINFFLG    =   1
                   MOVE    "0179"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *H25.2
           IF      SPA-ERRCD           =   SPACE
      *        訂正時の保険自動変換
               IF      SPA-HKN-ERR2        =   1
                   MOVE    "K176"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-HKN-ERR2
               END-IF
           END-IF
      *
      *    診療行為入力チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41011-KOUI-ALLHEN-SEC
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *
      *    診療日付のチェック訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02N-SRYYMD     NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
                   GO      TO      41001-KIHON-CHK-EXT
               END-IF
               IF     (SPA-K02N-NYUINKIKAN     =   SPACE ) AND
                      (SPA-GMN-MAX             >   ZERO  )
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "9033"              TO  SPA-ERRCD
                   GO      TO      41001-KIHON-CHK-EXT
               END-IF
           ELSE
      *
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
      *            誕生日以前の時、エラー
                   IF      SGDAY-SDAY          <   SPA-BIRTHDAY
                        MOVE    "1011"         TO  SPA-ERRCD
                        MOVE    7              TO  SPA-GMN-CUR
                   END-IF
      *            変更のない時、以降の処理はそのまま
                   IF      SGDAY-SDAY      NOT =   SPA-NAI-SRYYMD
                        MOVE    SGDAY-SDAY     TO  SPA-NAI-SRYYMD
                        GO      TO      41001-KIHON-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "1010"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  IDX-SET
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   SPA-MAX-LINE)  OR
                              (SPA-ERRCD        NOT =   SPACE )   OR
                              (SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF     (SPA-COM-CHKFLG (IDX)    =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)
                                           NOT =   "."  AND "S"
                                                        AND "*" )
      *            入院日の最後の日を診療日とする
                   MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
                   PERFORM VARYING IDX-N2      FROM    1   BY  1
                           UNTIL   IDX-N2      >   31
                       IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                           MOVE    IDX-N2     TO  SPA-SRYYMD(7:2)
                       END-IF
                   END-PERFORM
      *
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG 
      *            警告チェックあり
                   MOVE    "1"                 TO  LNK-SC92-KEIFLG
                   MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *
      *            小児科外来診療科の時、年齢エラーはそのまま
      *            IF      SPA-SYONIKA-ARI     NOT =   ZERO
      *                IF      LNK-SC92-ERRCD5 NOT =   SPACE
      *                    MOVE    SPACE           TO  LNK-SC92-ERRCD5
      *                END-IF
      *            END-IF
      *
                   PERFORM VARYING     IDX-ERR FROM    1  BY  1
                           UNTIL      (IDX-ERR     >   10     )  OR
                                      (SPA-ERRCD   NOT =   SPACE)
                       IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                           MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                       TO  SPA-ERRCD
                           IF      IDX-ERR             =   3 OR 6
                               MOVE    LNK-SC92-ERRMSG TO  SPA-ERRMSG2
                           END-IF
                       END-IF
                   END-PERFORM
      *---(01.00.01) LINE ADD  START  ----
      *            警告判定
                   IF      SPA-ERRCD(1:1)      =   "K"
                       IF      SPA-COM-KZMFLG (IDX)  =   SPACE
                           MOVE    "1"         TO  SPA-COM-KZMFLG(IDX)
                       ELSE
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
      *---(01.00.01) LINE ADD  END    ----
                   IF      SPA-ERRCD       NOT =   SPACE
                       IF      SPA-ERRCD(1:1)      =   "K"
                           MOVE    "2"             TO  SPA-COM-CUR(IDX)
                       ELSE
                           IF      SPA-COM-SET(IDX)    =   SPACE
                               MOVE    "1"         TO  SPA-COM-CUR(IDX)
                           ELSE
                               MOVE    "S001"      TO  SPA-ERRCD
                               MOVE    "1"         TO  SPA-COM-CUR
                                                           (IDX-SET)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
                   MOVE   IDX              TO  IDX-SET
               END-IF
           END-PERFORM
           MOVE    WRK-SPA-SRYYMD          TO  SPA-SRYYMD
      *
           .
       41001-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    途中終了画面へ（Ｋ１０）
      *****************************************************************
       4110S-K10-SYORI-SEC            SECTION.
      *
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK10"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    存在しない時、メッセージ表示
           IF      SPA-K10-MAX         =   ZERO
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    "1019"          TO  SPA-ERRCD
               ELSE
                   MOVE    "1018"          TO  SPA-ERRCD
               END-IF
               GO      TO      4110S-K10-SYORI-EXT
           END-IF
      *
           MOVE    "K10"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K10"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4110S-K10-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
             OR   (FLG-KINKI           =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
      *H30.9
             OR   (FLG-END             =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *    新規で明細がない時、エラー
           IF     (SPA-GMN-MAX         =   ZERO )  AND
                  (SPA-SYORIFLG        =   ZERO )
               MOVE    "9301"          TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    禁忌更新前のチェック
           PERFORM 41001-YAKKNK-CHK-SEC
      *
      *     禁忌エラーの時、エラー表示へ
           IF     (FLG-KINKI           =   1   )  AND
                  (FLG-END             =   1   )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    更新処理
      *****MOVE    SPACE           TO  WRK-PUTTYPE
           PERFORM 41002-KOUSIN-SYORI-SEC
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    診療室入力の時、ワーク行為負担データを作成し、選択へ
           IF      SPA-BUNSAN          =   "1"
               PERFORM 45109-SNRYO-NYU-SEC
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "2"                 TO  ORCSC100-KBN
           MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
           MOVE    FLG-INIT            TO  ORCSC100-INIT
           MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
           MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
           CALL    "ORCSC100"          USING
                                       ORCSC100AREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
      *H28.2
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *!!!!!!
      *    再度、排他のチェック
           IF      SPA-LOCK            =   ZERO
               MOVE    1                   TO  SLOCK-MODE
               MOVE    SPA-PTID            TO  SLOCK-PTID
               CALL    "ORCSLOCK"          USING
                                           MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
               MOVE    ZERO            TO  SPA-LOCK
               IF      SLOCK-RETURN        =   10
      *            排他中
                   MOVE    "9997"              TO  SPA-ERRCD
                   GO      TO      41002-KOUSIN-SYORI-EXT
               END-IF
           END-IF
      *!!!!!!
      *
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
      *
           MOVE    "K08"               TO  SPA-NAI-SAKIPG
      *****MOVE    "K03"               TO  SPA-SAKIPG
           MOVE    SPA-NAI-SAKIPG      TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    SPA-NAI-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45109-SNRYO-NYU-SEC              SECTION.
      *
      *    入院指示せん発行チェック
           PERFORM 451091-SIJISEN-CHK-SEC
      *    ユーザプログラム起動
           PERFORM 451092-USERPG-CHK-SEC
      *
           MOVE    ZERO                TO  FLG-SIJISEN-OK
           IF     (SPA-NAI-SIJISENKBN  =   1  )  AND
                 ((SPA-NAI-SYOHOU      =   1 )  OR
                  (SPA-NAI-CHUSYA      =   1 )  OR
                  (SPA-NAI-SIJISEN     =   1 ))
               MOVE    1                   TO  FLG-SIJISEN-OK
           END-IF
           IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *        ユーザプログラム起動あり
               MOVE    1                   TO  FLG-SIJISEN-OK
           END-IF
           IF      FLG-SIJISEN-OK       =   1
      *        入院指示せん発行あり
               PERFORM 450192-K081-SYORI-SEC
           ELSE
      *        確認メッセージ
               MOVE    "0117"              TO  SPA-KIDCD
               MOVE    SPACE               TO  SPA-KID1-FLG
           END-IF
      *
           .
       45109-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    入院指示せん発行チェック
      *****************************************************************
       451091-SIJISEN-CHK-SEC              SECTION.
      *
           INITIALIZE                  SPA-K081
      *    システム管理検索
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                  SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    "01"                TO  SYS-5007-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-5007-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-5007-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-5007-REC
               INITIALIZE                  SYS-5007-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
           ELSE
               INITIALIZE                  SYS-5007-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SYS-5007-SIJISENKBN =   "1"
               MOVE    1               TO  SPA-NAI-SIJISENKBN
           ELSE
               MOVE    ZERO            TO  SPA-NAI-SIJISENKBN
           END-IF
      *
      *    診療内容判定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
      *            薬評以外
             IF     (SPA-COM-YAKUHYO (IDX)   =   "1" )
      *            減点以外
             OR     (SPA-COM-GENTEN  (IDX)   =   "1" )
             OR     (SPA-GMN-INPUTCD (IDX)(1:1)
                                             =   "."
                                             OR  "*"  )
      *H28.9
      *    持参薬
              OR    (SPA-COM-JISANYAKU(IDX)  =   "1"
                                             OR  "2"  )
      *
               CONTINUE
             ELSE
               IF     (SPA-COM-SRYKBN (IDX)    =   "21"
                                               OR  "22"
                                               OR  "23" )
      *            院外処方は対象外
                   IF     (SPA-COM-SRYSYUKBN (IDX) =   "212"
                                                   OR  "222"
                                                   OR  "232"
      *H26.10           臨時追加
                                                   OR  "292"
                                                   OR  "295")
                       CONTINUE
                   ELSE
                       MOVE    1               TO  SPA-NAI-SYOHOU
                   END-IF
               END-IF
               IF     (SPA-COM-SRYKBN (IDX)    =   "14" ) AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
                                                   OR  "7")
      *            院外処方は対象外
                   IF     (SPA-COM-SRYSYUKBN (IDX) =   "148"
                                                   OR  "149" )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  SPA-NAI-SYOHOU
                   END-IF
               END-IF
      *
               IF     (SPA-COM-SRYKBN (IDX)    =   "31"
                                               OR  "32"
                                               OR  "33" ) AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
                                                   OR  "7")
                   MOVE    1               TO  SPA-NAI-CHUSYA
               END-IF
      *
               IF     (SPA-COM-SRYKBN (IDX)    =   "40"
                                               OR  "80" ) AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"
                                                   OR  "6" 
                                                   OR  "7")
                   MOVE    1               TO  SPA-NAI-SIJISEN
               END-IF
             END-IF
           END-PERFORM
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
      *
           .
       451091-SIJISEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動初期
      *****************************************************************
       451092-USERPG-CHK-SEC              SECTION.
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "K02N"              TO  ORCSUSERCHK-GMNPG
           MOVE    "1"                 TO  ORCSUSERCHK-SYORI
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
           MOVE    ORCSUSERCHK-GYOUMU-FLG  TO  SPA-NAI-GYOUMU-FLG
           MOVE    ORCSUSERCHK-GYOUMU-ARI  TO  SPA-NAI-GYOUMU-ARI
           .
       451092-USERPG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院指示せん指示画面へ（Ｋ０８１）
      *****************************************************************
       450192-K081-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
      *
      *    ユーザプログラム起動有無
           MOVE    SPA-NAI-GYOUMU-FLG  TO  SPA-K081NAI-GYOUMU-FLG
           MOVE    SPA-NAI-GYOUMU-ARI  TO  SPA-K081NAI-GYOUMU-ARI
           MOVE    "K02N"              TO  SPA-ETC-MOTOPG
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK081"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           MOVE    "K081"              TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-ETC-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K081"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450192-K081-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク行為負担データを作成、Ｋ０１へ
      *****************************************************************
       4510-SNRYO-NYU-SEC              SECTION.
      *
           MOVE    "1"                 TO  FLG-SYORI
           PERFORM 45101-SNRYO-SYORI-SEC
      *
      *    入院指示せん発行処理
           IF      FLG-SIJISEN         =   1
               PERFORM 45900-SIJISEN-SYORI-SEC
           END-IF
      *
      *    ユーザプログラム起動
           PERFORM 4907-USERPG-SEC
           IF     (FLG-END             =   1 )
             OR   (FLG-END2            =   1 )
               CONTINUE
           ELSE
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    患者ＩＤを初期化する
           INITIALIZE                  SPA-KYOTUKEY
      *
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           END-IF
      *
           .
       4510-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45101-SNRYO-SYORI-SEC              SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           IF      FLG-SYORI           =   "1"
               MOVE    "2"                 TO  ORCSCWKSRY-KBN
           ELSE
               MOVE    "3"                 TO  ORCSCWKSRY-KBN
           END-IF
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *    DBエラー
           IF      SPA-ERRCD           =   "1093"
                                       OR  "9999"
               MOVE    1               TO  FLG-END2
           END-IF
      *
           .
       45101-SNRYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院指示せん発行処理
      *****************************************************************
       45900-SIJISEN-SYORI-SEC            SECTION.
      *
      *    入院指示せんオーダーデータ作成
           INITIALIZE                  ORCSORDERPRTAREA
           MOVE    "K02N"              TO  LNK-ORDERPRT-PG
           IF      FLG-SYORI           =   "1"
      *        中途終了時
               MOVE    "1"                 TO  LNK-ORDERPRT-KBN
           ELSE
      *        パラメタより（印刷のみ）
               MOVE    "2"                 TO  LNK-ORDERPRT-KBN
           END-IF
      *
           IF      SPA-K081GMN-SYOHOU  =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-SYOHOU
           END-IF
           IF      SPA-K081GMN-CHUSYA  =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-CHUSYA
           END-IF
           IF      SPA-K081GMN-SIJISEN =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-SIJISEN
           END-IF
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  LNK-ORDERPRT-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  LNK-ORDERPRT-DRCD(2:4)
           END-IF
      *    同日再入院
           MOVE    SPA-K02N-DOUKBN     TO  LNK-ORDERPRT-DOUJITSUKBN
           CALL    "ORCSORDERPRT"      USING
                                       ORCSORDERPRTAREA
                                       SPA-AREA
           IF      LNK-ORDERPRT-RCD    NOT =   ZERO
               MOVE    "1200"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-END2
           END-IF
      *
      *    帳票プログラム取得
           PERFORM 459001-PRTNUM-HEN-SEC
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           IF    ((SPA-CLIENT-PRT-FLG  =   2      )
              OR  (SPA-PRTCONF         =   "1"    ))
             AND  (SPA-ERRCD           =   SPACE  )
      *        クライアント印刷制御情報取得処理
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    入院処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-SYOHOU  =   "1"    )
               INITIALIZE              ORCHC501AREA
               MOVE    SPA-GMN-PTNUM       TO  ORCHC501-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC501-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC501-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC501-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC501-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC501-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC501-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK1KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC501-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "01"                TO  SPA-PRT-FLG
               CALL    WRK-SYOHOU-PG       USING
                                           SPA-AREA
                                           ORCHC501AREA
               IF      ORCHC501-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    注射処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-CHUSYA  =   "1"    )
               INITIALIZE              ORCHC502AREA
               MOVE    SPA-GMN-PTNUM       TO  ORCHC502-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC502-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC502-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC502-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC502-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC502-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC502-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK2KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC502-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "02"                TO  SPA-PRT-FLG
               CALL    WRK-CHUSYA-PG       USING
                                           SPA-AREA
                                           ORCHC502AREA
               IF      ORCHC502-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    指示せん印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-SIJISEN =   "1"    )
               INITIALIZE              ORCHC503AREA
               MOVE    SPA-GMN-PTNUM       TO  ORCHC503-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC503-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC503-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC503-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC503-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC503-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC503-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK3KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC503-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "03"                TO  SPA-PRT-FLG
               CALL    WRK-SIJISEN-PG      USING
                                           SPA-AREA
                                           ORCHC503AREA
               IF      ORCHC503-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
           .
       45900-SIJISEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       459001-PRTNUM-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           MOVE    SPACE               TO  WRK-CHUSYA-PG
           MOVE    SPACE               TO  WRK-SIJISEN-PG
      *    入院処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1202"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
           END-IF
      *    注射処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1203"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-CHUSYA-PG
           END-IF
      *    指示せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1204"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SIJISEN-PG
           END-IF
           .
       459001-PRTNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      *    INITIALIZE                  KDMY01
      *    MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *    MOVE    "入院処方箋"        TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "注射処方箋"        TO  ORCSPRTCTRL-TITLE-I(02)
      *    MOVE    "入院指示箋"        TO  ORCSPRTCTRL-TITLE-I(03)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      **   MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      ***  MOVE    1                   TO  FLG-CLIENT-OK
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC  SECTION.
      *
      *    追加
           MOVE    1                   TO  SPA-SYORI-FLG
      *    ドクターは指示画面
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  SPA-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  SPA-DRCD(2:4)
           END-IF
           IF      SPA-K081GMN-USERPG  =   "1"
      *        ユーザプログラム実行画面表示へ
               PERFORM 49071-XD01-SYORI-SEC
           ELSE
               IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "K02N"              TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SRYYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
                   MOVE    SPA-PTNUM           TO  ORCSUSERCHK-PTNUM
                   MOVE    SPA-SRYKA           TO  ORCSUSERCHK-SRYKA
                   MOVE    SPA-HKNCOMBINUM     TO  ORCSUSERCHK-HKNCOMBI
                   MOVE    SPA-DRCD            TO  ORCSUSERCHK-DRCD
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
           .
       4907-USERPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム実行画面表示処理
      *****************************************************************
       49071-XD01-SYORI-SEC              SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *    共通キー編集
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPACE               TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "XD01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       49071-XD01-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *
      *    相互作用チェック期間
           MOVE    ZERO                TO  FLG-KINKI
           IF      SPA-INTERACT-KIKAN  >   ZERO
               INITIALIZE                  ORCSCKNKCHKAREA
               MOVE    "3"                 TO  ORCSCKNKCHK-KBN 
               CALL    "ORCSCKNKCHK"       USING
                                           ORCSCKNKCHKAREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               IF      SPA-K021-MAX        >   ZERO
                   MOVE    1                   TO  FLG-KINKI
                   MOVE    2                   TO  FLG-ALLFLG
                   MOVE    "K03"               TO  SPA-K021-SAKI
                   PERFORM 51020-K021-SYORI-SEC
               END-IF
           END-IF
      *
           .
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示処理
      *****************************************************************
       51020-K021-SYORI-SEC              SECTION.
      *
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K021"              TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K021"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       51020-K021-SYORI-EXT.
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
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   1
               MOVE    "1027"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG2
           END-IF
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   2
               MOVE    "1070"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG2
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-HKN-ERR         =   1
               MOVE    "1028"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG2
           END-IF
           IF      SPA-HKN-ERR         =   2
               MOVE    "1032"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG2
           END-IF
      *    保険未選択の場合、エラー
           IF      SPA-HKN-ERR         =   3
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG2
           END-IF
      *    保険選択エラーの場合
           IF      SPA-HKN-ERR         =   4
               MOVE    "1034"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *    退職国保の年齢
           IF      SPA-RJN-ERR         =   9
               MOVE    "1077"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG2
           END-IF
      *H24.9
      *    アフターケアの中途データ展開
           IF      SPA-HKN-ERR         =   9
               IF      SPA-KIDCD           =   "0116"
                                           OR  "0127"
      *            クリアは行う
                   CONTINUE
               ELSE
                   MOVE    "9041"              TO  SPA-ERRCD
                   MOVE    SPACE               TO  SPA-ERRMSG2
               END-IF
           END-IF
      *
      *    入院料設定ないエラー
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NYU-ERR         =   1
                   MOVE    "0217"              TO  SPA-ERRCD
                   IF      SPA-LOCK            =   ZERO
                       MOVE    20                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *!!!!
      *    後期高齢者の保険がない時
      *!!! ７５歳到達を警告へ
           IF     (SPA-RJN-ERR         =   3   )  AND
                  (SPA-ERRCD           =   SPACE )
               IF      SPA-NAI-ROUSTR      =   ZERO
                   MOVE    "1076"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-NAI-ROUSTR
               END-IF
           END-IF
      *!!!!
      *    ７０歳到達の警告
           IF      SPA-ERRCD       =   SPACE
               IF      SPA-NAI-ROUFLG      =   1
                   IF      SPA-NAI-ROUSTR      =   ZERO
                       MOVE    1               TO  SPA-NAI-ROUSTR
                       MOVE    "K008"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    数量ゼロの場合、エラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *    訂正時の登録エラー
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-TEISEI-CHK      =   ZERO)
               IF      SPA-ERRCD       NOT =   "9100"
                   MOVE    "K999"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-TEISEI-CHK
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    地方公費のみの場合、警告
           IF     (SPA-NAI-KOHFLG      =   1   )  AND
                  (SPA-ERRCD           =   SPACE
                                       OR  "K034" )
               IF      SPA-NAI-KOHFLG-ERR  =   ZERO
                   MOVE    "K034"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KOHFLG-ERR
               END-IF
           END-IF
      *    国保 前期高齢者変更チェック
           IF     (SPA-NAI-ZENKIFLG    =   1   )  AND
                  (SPA-ERRCD           =   SPACE
                                       OR   "K171")
               IF      SPA-NAI-ZENKIFLG-ERR    =   ZERO
                   MOVE    "K171"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-ZENKIFLG-ERR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        訂正時の保険自動変換
               IF      SPA-HKN-ERR2        =   1
                   MOVE    "K176"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-HKN-ERR2
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-JYURRKFLG   =   1
                   MOVE    "0178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JYURRKFLG   =   2
                   MOVE    "K178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
                   MOVE    ZERO                TO  SPA-NAI-JYURRKFLG
               END-IF
           END-IF
      *
      *R02.1
      *        患者登録で変更
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-PTINFFLG    =   1
                   MOVE    "0179"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-LOCK        =   2
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-LOCK
               END-IF
           END-IF
      *
      *    コメント入力位置エラーはカーソル移動のみ
           IF      SPA-ERRCD           =   "0100"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *!!
      *    中途データＤＢ更新エラー表示
           IF      SPA-K01-YOBI(1:1)   =   "E"
               MOVE    "1999"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-K01-YOBI(1:1)
               MOVE    90              TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD           =   "1999"
               MOVE    90              TO  SPA-GMN-CUR
           END-IF
      *!!
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD2      NOT =   SPACE
               PERFORM 530-KID2SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K02N"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
      *****MOVE    ZERO                TO  SPA-GMN-MAX
      *ver.4.7
           MOVE    ZERO                TO  WRK-MAP-CUR4
           MOVE    ZERO                TO  FLG-MAP-CUR4
      *ver.4.7
      *訂正展開時、１行目にカーソル移動
           IF      SPA-GMN-CUR         =   98
               IF      SPA-GMN-MAX     >=  SPA-GMLINCNT
      *        ２８行以上であれば１行目へ
                   MOVE    "1"             TO  SPA-COM-CUR (1)
               END-IF
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
      *ページなし
      ****         IF      IDX               >   MAX-GMN
      *                ADD     1             TO  WRK-PAGE
      *                MOVE    1             TO  IDX
      ****         END-IF
      *
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *********        前・次ページの時
      *                IF     (SPA-GMN-CUR         =   88  OR  99 )
      *                    IF      SPA-COM-CUR (WRK-IDX)   =   "1" 
      *                       AND (SPA-ERRCD      NOT =   SPACE )
      *                        IF      WRK-MAP-CUR         =   ZERO
      *                            MOVE    IDX        TO  WRK-MAP-CUR
      *                            MOVE    WRK-PAGE   TO  WRK-MAP-PAGE
      *                        END-IF
      *                    END-IF
      *********        ELSE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"  OR "3"
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
      *ver.4.7
      *                今回入力位置
                       IF     (SPA-COM-IN4 (WRK-IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR4
                           MOVE    1               TO  FLG-MAP-CUR4
                       END-IF
                       IF     (SPA-COM-IN4 (WRK-IDX)   =   "2" )
                           MOVE    IDX             TO  WRK-MAP-CUR4
                           MOVE    2               TO  FLG-MAP-CUR4
                       END-IF
      *
      *
      ************     END-IF
      *
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
      *************IF      SPA-ERRCD       =   SPACE
                   IF      SPA-COM-CUR (WRK-IDX)   =   "1"
      *!!
      *            セットで警告の時
                       IF     (SPA-GMN-INPUTCD (WRK-IDX)(1:1) =  "S")
                          AND (SPA-ERRCD(1:1)      =   "K"   )
                          AND ((SPA-COM-KZMFLG4 (WRK-IDX)  =  "1" )
                           OR  (SPA-COM-KZMFLG5 (WRK-IDX)  =  "1" )
                           OR  (SPA-COM-KINKIFLG(WRK-IDX)  =  "1" )
                           OR  (SPA-COM-TUKIJGNFLG(WRK-IDX)  =  "1" )
                           OR  (SPA-COM-TUKIJGNFLG1(WRK-IDX) =  "1" ))
                          CONTINUE
                       ELSE
                           MOVE    SPACE       TO  SPA-MAE-INPUTCD
                                                            (WRK-IDX)
                       END-IF
                   ELSE
                       IF      SPA-COM-CUR (WRK-IDX)   NOT =   "2"
                           MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
                       END-IF
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    WRK-IDX         TO  SPA-GMN-MAX
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
      *ver.4.7
      *    カーソル指定のない時、入力行へ
           IF     (WRK-MAP-CUR4    NOT =   ZERO )
            AND   (WRK-MAP-MAX         >   WRK-MAP-CUR4)
               MOVE    WRK-MAP-CUR4        TO  SPA-MAP-IDX
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    FLG-MAP-CUR4        TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *    カーソル指定のない時、次ぎの空白行へ
           COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                       +   1
           MOVE    1                   TO  SPA-GMN-PAGE
      *
      *
      *
           GO      TO      5002-GMNSPA-HEN-EXT
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
               IF      SPA-SINSATU-SAI     =   ZERO
      *            初期表示の時１ページ目へ
                   IF     (SPA-MAP-IDX         >   MAX-GMN  )
                       OR (SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE)
      *ver.4.7 ******  MOVE    99                  TO  SPA-MAP-IDX
                       MOVE    MAX-GMN             TO  SPA-MAP-IDX
                   ELSE
                       IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
                           MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO            TO  SPA-SINSATU-SAI
      *            最終行へ
                   IF     (SPA-MAP-IDX         >   MAX-GMN )
      *ver.4.7 ******* MOVE    99                  TO  SPA-MAP-IDX
                       MOVE    MAX-GMN             TO  SPA-MAP-IDX
                   ELSE
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
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-GMN-MAX1        =   ZERO )
               MOVE    ZERO                TO  WRK-K02-ROW
           ELSE
               MOVE    K02N-ROW            TO  WRK-K02-ROW
           END-IF
      *
           MOVE    SPACE               TO  K02N
           INITIALIZE                      K02N
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  K02N-ROW
           MOVE    ZERO                TO  K02N-ROWATTR
           IF      WRK-K02-ROW     NOT =   ZERO
               MOVE    WRK-K02-ROW         TO  K02N-ROW
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  FLG-BGCOLORS
           MOVE    ZERO                TO  IDX2
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   SPA-MAX-LINE  )  OR
                              (IDX         >=  MAX-GMN   )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1                   TO  IDX
      *ver.4.7
      *            スクロール背景色（１行毎）
                   IF      FLG-BGCOLORS        =   ZERO
      *                白色設定
                       MOVE    CL-WHITE        TO
                                       K02N-SRYKBN-BGCOLOR  (IDX)
                                       K02N-INPUTCD-BGCOLOR (IDX)
                                       K02N-MEISYO-BGCOLOR  (IDX)
                                       K02N-SURYO-BGCOLOR   (IDX)
                       IF      SPA-BGLINEKBN       =   "1"
      *                    剤毎
                           IF     (SPA-COM-ZAIEND(WRK-IDX) =  "1"  )
                            AND  ((WRK-IDX         <  MAX-GMN  )  AND
                                  (SPA-GMN-INPUTCD(WRK-IDX + 1)
                                                   NOT =  SPACE))
                               MOVE    1               TO  FLG-BGCOLORS
                           END-IF
                       ELSE
      *                    行毎
                           MOVE    1               TO  FLG-BGCOLORS
                       END-IF
                   ELSE
                       MOVE    SPA-BGCOLORS        TO
                                       K02N-SRYKBN-BGCOLOR (IDX)
                                       K02N-INPUTCD-BGCOLOR(IDX)
                                       K02N-MEISYO-BGCOLOR (IDX)
                                       K02N-SURYO-BGCOLOR  (IDX)
                       IF      SPA-BGLINEKBN       =   "1"
      *                    剤毎
                           IF     (SPA-COM-ZAIEND(WRK-IDX) =  "1"  )
                             OR  ((WRK-IDX         <  MAX-GMN  )  AND
                                  (SPA-GMN-INPUTCD(WRK-IDX + 1)
                                                   =  SPACE))
                               MOVE    ZERO            TO  FLG-BGCOLORS
                           END-IF
                       ELSE
      *                    行毎
                           MOVE    ZERO            TO  FLG-BGCOLORS
                       END-IF
                   END-IF
      *!!
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  K02N-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  K02N-INPUTCD(IDX)
      *
      *TEST
      *H30.9       選択式コメントあり
                   IF      SPA-COM-RECEKISAI (WRK-IDX) NOT =  SPACE
                       MOVE   "S"       TO  SPA-GMN-MEIH (WRK-IDX)(2:1)
                   END-IF
      *
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  K02N-MEISYO(IDX)
      *H29.1
      *!!          ユーザー管理
                   IF      SPA-COM-MASTER-CLASS (WRK-IDX)  =   "2"
                       MOVE    CL-GREEN        TO
      *?                              K02N-SRYKBN-FGCOLOR (IDX)
                                      K02N-INPUTCD-FGCOLOR(IDX)
      *?                              K02N-MEISYO-FGCOLOR(IDX)
      *?                              K02N-SURYO-FGCOLOR(IDX)
                   END-IF
      *TEST
                   PERFORM 5002-SURYO-HEN-SEC
      *@
      *            包括行
                   IF     (SPA-COM-HOUKATU-ZAI (WRK-IDX) =   1 )
                      OR  (SPA-COM-HOUKATU-ERR (WRK-IDX) NOT =   ZERO)
      *ver.4.7
                       MOVE    CL-BLUE        TO  K02N-SRYKBN-FGCOLOR
                                                                (IDX)
                       MOVE    CL-BLUE        TO  K02N-MEISYO-FGCOLOR
                                                                 (IDX)
                       IF      SPA-COM-HOUKATU-FLG2(WRK-IDX)   =   1
                           MOVE    CL-RED         TO
                                       K02N-SRYKBN-FGCOLOR (IDX)
                                       K02N-MEISYO-FGCOLOR (IDX)
                       END-IF
                   ELSE
                       MOVE    CL-BLACK       TO  K02N-SRYKBN-FGCOLOR
                                                                 (IDX)
                       MOVE    CL-BLACK       TO  K02N-MEISYO-FGCOLOR
                                                                 (IDX)
                   END-IF
      *!!          禁忌薬剤
                   IF      SPA-COM-PTKINKI-ARI (WRK-IDX)   =   1
                       MOVE    CL-RED         TO  K02N-MEISYO-FGCOLOR
                                                                 (IDX)
                   END-IF
      *!
      *             最終行
                    IF      K02N-INPUTCD (IDX) NOT =   SPACE
                        MOVE    IDX                TO  IDX2
                    END-IF
               ELSE
      *            剤毎（約束セット対応）
                   IF     (SPA-BGLINEKBN       =   "1" )  AND
                          (SPA-COM-ZAIEND(WRK-IDX) =  "1"  )
                       IF      FLG-BGCOLORS        =   ZERO
                           MOVE    1               TO  FLG-BGCOLORS
                       ELSE
                           MOVE    ZERO            TO  FLG-BGCOLORS
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ページ表示
      *    MOVE    SPACE               TO  WRK-HEN-PAGE
      *    MOVE    "頁"                TO  WRK-HEN1
      *    IF      WRK-MAP-MAXPAGE     >   ZERO
      *        MOVE    SPA-GMN-PAGE        TO  WRK-HEN-PAGE1
      *        MOVE    "/"                 TO  WRK-HEN2
      *        MOVE    WRK-MAP-MAXPAGE     TO  WRK-HEN-PAGE2
      *    END-IF
      *    MOVE    WRK-HEN-PAGE        TO  K02N-MODE
      *ver.4.7
      *    行数表示
           MOVE    SPACE               TO  K02N-MODE
           MOVE    IDX2                TO  WRK-HEN-PAGE1
           STRING  "行数:"
                   WRK-HEN-PAGE1       DELIMITED  BY  SIZE
                                       INTO      K02N-MODE
           END-STRING
      *
      *    前頁表示の時、１行目へ
      *    IF      SPA-GMN-CUR         =   99  OR  88
      *        MOVE    1                   TO  SPA-GMN-CUR
      *    END-IF
      *
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN-CUR         =   2
               IF      K02N-INPUTCD(SPA-MAP-IDX)    =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  K02N-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  K02N-KANANAME
           MOVE    SPA-GMN-NAME        TO  K02N-NAME
      *    排他中
           IF      SPA-LOCK                =   ZERO
               MOVE    "black"             TO  K02N-KANANAME-STYLE
               MOVE    "black"             TO  K02N-NAME-STYLE
           ELSE
               MOVE    "red"               TO  K02N-KANANAME-STYLE
               MOVE    "red"               TO  K02N-NAME-STYLE
           END-IF
           MOVE    SPA-GMN-SEX         TO  K02N-SEX
           MOVE    SPA-GMN-HKNCOMBI-G
                                       TO  K02N-HKNCOMBI
           MOVE    SPA-GMN-FTNRATEMEI  TO  K02N-RATE
      *    診療日
           IF      SPA-GMN-SRYYMD      =   SPACE
               MOVE    SPA-SRYYMDW         TO  K02N-SRYYMD
           ELSE
               MOVE    SPA-GMN-SRYYMD      TO  K02N-SRYYMD
           END-IF
           MOVE    SPA-GMN-BIRTHDAY    TO  K02N-BIRTHDAY
      *    年齢
           MOVE    SPA-NENREI-HEN      TO  K02N-NENREI
      *
      *    保険組合せ
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-HKN-LIST (IDX)  TO  K02N-HKNCOMBI-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  K02N-HKNCOMBI-COUNT
      *
      *    診療科
           MOVE    SPA-GMN-SRYKA-G     TO  K02N-SRYKA
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  K02N-SRYKA-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  K02N-SRYKA-COUNT
      *
      *    入院期間  
           MOVE    SPA-K02N-NYUINKIKAN TO  K02N-NYUINKIKAN
           IF      SPA-K02N-DOUNYUKBN  =   1
      *        同日再入院有
               MOVE    "blue"            TO  K02N-NYUINKIKAN-STYLE
           ELSE
               MOVE    "black"           TO  K02N-NYUINKIKAN-STYLE
           END-IF
      *    入院科 
           MOVE    SPA-K02N-NYUINKAMEI TO  K02N-NYUINKA
      *
      *    時間外区分
           MOVE    SPA-K02N-JIKANKBN-G TO  K02N-JIKANKBN
           PERFORM  VARYING   IDX      FROM    1   BY  1
                    UNTIL     IDX      >   5
               MOVE    SPA-K02N-JIKANKBN-LST(IDX)
                                       TO  K02N-JIKANKBN-ITEM (IDX)
           END-PERFORM
           MOVE    5                   TO  K02N-JIKANKBN-COUNT
      *
      *    ＤＯ診療日
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX1
               MOVE    SPA-GMN-NO     (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K02N-NO  (IDX)
               MOVE    SPA-GMN-RRKYMD (IDX)    TO  K02N-RRKYMD(IDX)
               MOVE    SPA-GMN-RRKHKNCOMBI(IDX)
                                               TO  K02N-RRKHKNCOMBI(IDX)
               MOVE    SPA-GMN-RRKSRYKA(IDX)   TO  K02N-RRKSRYKA(IDX)
      *D       IF      SPA-GMN-NO (IDX)    =   SPA-NAI-TESELNUM
               IF      IDX                 =   SPA-NAI-TESELNUM
                   MOVE    "T"             TO  K02N-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  K02N-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX1        TO  K02N-COUNT
      *
           MOVE    SPA-GMN-DOSELNUM    TO  K02N-DOSELNUM
      *
      *    訂正・処理
           IF      SPA-GMN-TEISEI      =   "T"
      *        訂正診療日選択以外の時、ＤＯ検索へ戻す
               IF      SPA-GMN-CUR         NOT =   4
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    ZERO            TO  SPA-NAI-SYORI
               END-IF
           END-IF
           EVALUATE    SPA-GMN-TEISEI
               WHEN    "T"
                   MOVE    "T"             TO  K02N-TEISEI-VALUE
                   MOVE    "訂正診療日"    TO  K02N-TEISEI-LABEL
                   MOVE    1               TO  SPA-NAI-SYORI
                   MOVE    "T"             TO  SPA-GMN-TEISEI
               WHEN    "F"
               WHEN    SPACE
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    "F"             TO  K02N-TEISEI-VALUE
                   MOVE    "ＤＯ検索"      TO  K02N-TEISEI-LABEL
                   MOVE    ZERO            TO  SPA-NAI-SYORI
           END-EVALUATE
      *
           EVALUATE    SPA-K02N-KENSAFLG
               WHEN    "T"
                   MOVE    "T"             TO  K02N-KENSA-VALUE
                   MOVE    "検査まとめ"    TO  K02N-KENSA-LABEL
               WHEN    "F"
               WHEN    SPACE
                   MOVE    "F"             TO  SPA-K02N-KENSAFLG
                   MOVE    "F"             TO  K02N-KENSA-VALUE
                   MOVE    "検査追加"      TO  K02N-KENSA-LABEL
           END-EVALUATE
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K02N-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K02N-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K02N-SYORIMEI-VALUE
           END-EVALUATE
      *
      *    継続
           EVALUATE    SPA-CONTKBN
               WHEN    SPACE
                   MOVE    SPACE           TO  K02N-CONTKBN-VALUE
               WHEN    "1"
                   MOVE    "red"           TO  K02N-CONTKBN-STYLE
                   MOVE    "［継］"        TO  K02N-CONTKBN-VALUE
               WHEN    "2"
                   MOVE    "red"           TO  K02N-CONTKBN-STYLE
                   MOVE    "［確］"        TO  K02N-CONTKBN-VALUE
           END-EVALUATE
      *
      *    リハビリ・患者情報編集
           PERFORM 5003-TABBOHEN-SEC
           MOVE    IDY                 TO  K02N-TABOO-COUNT
           MOVE    K02N-TABOO-ITEM(1)  TO  K02N-TABOO
      *
      *    他院紐付けあり
           IF      SPA-NAI-GRPKBN      =   1
               MOVE    "darkgreen"         TO  K02N-GRPMEI-STYLE
               MOVE    "【他】"            TO  K02N-GRPMEI-VALUE
           ELSE
               MOVE    "black"             TO  K02N-GRPMEI-STYLE
               MOVE    SPACE               TO  K02N-GRPMEI-VALUE
           END-IF
      *----(04.07.00) LINE UPD START ----------------------------------
      *    医院名称
      *     MOVE    SPA-HOSPNAME        TO  K02N-TITLE
           MOVE    SPACE           TO  K02N-TITLE
           STRING  SPA-HOSPNAME        DELIMITED BY SPACE
                   "  ["               DELIMITED BY SIZE
                   SPA-OPID            DELIMITED BY SPACE
                   "]"                 DELIMITED BY SIZE
                                 INTO  K02N-TITLE
           END-STRING
      *----(04.07.00) LINE UPD END   ----------------------------------
      *
      *ver.4.8
      *    患者情報通知
           MOVE    SPA-GMN-POPUP-SUMMARY   TO  K02N-POPUP-SUMMARY
           MOVE    SPA-GMN-POPUP-BODY      TO  K02N-POPUP-BODY
           MOVE    SPA-GMN-POPUP-ICON      TO  K02N-POPUP-ICON
           MOVE    SPA-GMN-POPUP-TIMEOUT   TO  K02N-POPUP-TIMEOUT
           IF      SPA-GMN-POPUP-SUMMARY   NOT = SPACE
               INITIALIZE SPA-GMN-POPUP-STATUS
           END-IF
      *
      *H29.5
      *    メモボタン
           IF      SPA-NAI-MEMOKBN     =   "1"
               MOVE    "red"               TO  K02N-BSH2-STYLE
           ELSE
               MOVE    "black"             TO  K02N-BSH2-STYLE
           END-IF
           MOVE    "メモ"              TO  K02N-BSH2-LABEL
      *    公費期限切れ名称
           IF      SPA-GMN-KOHMSG      =   SPACE
               MOVE    SPACE               TO  K02N-KOHMSG-DATA
           ELSE
               MOVE    "red"               TO  K02N-KOHMSG-STYLE
               MOVE    SPA-GMN-KOHMSG      TO  K02N-KOHMSG-DATA
           END-IF
      *
      *H30.4
           IF     (SPA-SRYYMD          >=  "20180401")
               IF     (SPA-NINPUKBN-ARI    =   1    )
                   MOVE    "（妊婦）"      TO  K02N-MODE3-VALUE
                   MOVE    "blue"          TO  K02N-MODE3-STYLE
               ELSE
      *            手入力で妊婦
                   IF     (SPA-NINPUKBN-IN     =   1    )
                   MOVE    "（妊婦）"      TO  K02N-MODE3-VALUE
                   MOVE    "black"         TO  K02N-MODE3-STYLE
                   END-IF
               END-IF
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           IF      FLG-END             =   ZERO
      *         カーソルセット
                PERFORM 5001-MAPCUR-SEC
            END-IF
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *業務処理権限設定
      *    受付
           IF      SPA-GSRAUTH-11      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B04S-STATE
      *ver.4.7  照会からは非活性
               IF      SPA-MACHIPG          =   "Q02"
                                            OR  "Q02A"
                                            OR  "Q02B"
                   MOVE    WIDGET-INSENSITIVE  TO  K02N-B04S-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B04S-STATE
           END-IF
      *    患者登録
           IF      SPA-GSRAUTH-12      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B05S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B05S-STATE
           END-IF
      *    病名登録
           IF      SPA-GSRAUTH-22      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B07S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B07S-STATE
           END-IF
      *    収納登録
           IF      SPA-GSRAUTH-23      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B08S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B08S-STATE
           END-IF
      *    会計照会
           IF      SPA-GSRAUTH-24      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B09S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B09S-STATE
           END-IF
      *    排他中
           IF      SPA-LOCK            =   ZERO
               MOVE    WIDGET-NORMAL       TO  K02N-B12-STATE
               MOVE    WIDGET-NORMAL       TO  K02N-B12S-STATE
               MOVE    WIDGET-NORMAL       TO  K02N-B06S-STATE
               MOVE    WIDGET-NORMAL       TO  K02N-B11S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B04S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B05S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B07S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B08S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B09S-STATE
      *
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B12S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B06S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B11S-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
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
      *    回数
           MOVE    ZERO                TO  IDK2
      *    外用薬の時、１
           IF     (SPA-COM-SRYKBN (WRK-IDX)    =   "23" )  AND
                  (SPA-GMN-KAISU  (WRK-IDX)    NOT =   SPACE )
               MOVE    1                   TO  WRK-KAISU-Z
           ELSE
               MOVE    SPA-GMN-KAISU  (WRK-IDX)    TO  WRK-KAISU-Z
           END-IF
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           IF      SPA-GMN-KAISU (WRK-IDX) =   SPACE
               MOVE    SPACE               TO  WRK-KAKERU
           ELSE
               IF     (SPA-GMN-KEI   (WRK-IDX) =   ZERO )
      *            院外処方の時、回数表示
                   IF     (SPA-COM-SRYKBN(WRK-IDX)
                                               =   "21"  OR "22"
                                               OR  "23" )   OR
                          (SPA-COM-SRYSYUKBN(WRK-IDX)
                                               =   "148"  OR
                                                   "149"  )
                       MOVE    " X "               TO  WRK-KAKERU
      *ver.4.7
                       IF      SPA-COM-INGAITEN (WRK-IDX) >  ZERO
                           MOVE    SPACE           TO  WRK-TENSU-GX
                           MOVE    SPA-COM-INGAITEN(WRK-IDX)
                                                   TO  WRK-TENSU-GZ
                           MOVE    "("             TO  WRK-TENSU-G1
                           MOVE    ")"             TO  WRK-TENSU-G2
                           MOVE    WRK-TENSU-GX    TO  WRK-TENSU-X
                        END-IF
      *ver.4.7
                   ELSE
                       MOVE    SPACE               TO  WRK-KAISU-H
                       MOVE    SPACE               TO  WRK-KAKERU
                   END-IF
               ELSE
                   MOVE    " X "               TO  WRK-KAKERU
               END-IF
           END-IF
           STRING  WRK-TENSU-X             DELIMITED   BY  SIZE
                   WRK-KAKERU              DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI-H
           END-STRING
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
           MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
           MOVE    WRK-ZAIKEI-H                TO  WRK-H-ZAIKEI
           IF      SPA-GMN-KEI (WRK-IDX)   >   ZERO
               MOVE    SPA-GMN-KEI (WRK-IDX)
                                           TO  WRK-KEI-Z
               MOVE    WRK-KEI-Z           TO  WRK-H-KEI
           ELSE
               MOVE    SPA-GMN-KEI (WRK-IDX)
                                           TO  WRK-KEI-Z2
               MOVE    WRK-KEI-Z2          TO  WRK-H-KEI
           END-IF
      *
           MOVE    WRK-SURYO-HENSYU    TO  K02N-SURYO (IDX)
      **** MOVE    "user-font"         TO  K02N-SURYO-STYLE(IDX)
      *@
      *    包括剤
           IF    ( SPA-COM-HOUKATU-ZAI (WRK-IDX) =   1  ) 
             OR  ( SPA-COM-HOUKATU-ERR (WRK-IDX) NOT =   ZERO)
              MOVE    CL-BLUE        TO  K02N-SURYO-FGCOLOR (IDX)
           END-IF
      *
           .
       5002-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・患者情報編集処理
      *****************************************************************
       5003-TABBOHEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX2    FROM    1   BY  1
     **            UNTIL       IDX2    >   5
                   UNTIL       IDX2    >   6
               MOVE    ZERO                TO  IDY2
               EVALUATE    SPA-GMNHYOKBN (IDX2)
               WHEN    "1"
      *            リハビリ
                   MOVE    9              TO  IDY2
               WHEN    "2"
      *            禁忌
                   MOVE    1              TO  IDY2
               WHEN    "3"
      *            禁忌
                   MOVE    2              TO  IDY2
               WHEN    "4"
      *            禁忌
                   MOVE    3              TO  IDY2
               WHEN    "5"
      *            禁忌
                   MOVE    4              TO  IDY2
      *H28.10
               WHEN    "6"
      *            介護
                   MOVE    6              TO  IDY2
               END-EVALUATE
      *
               EVALUATE    IDY2
               WHEN    9
      *            リハビリ開始日
                   PERFORM  VARYING   IDX     FROM    1   BY  1
                            UNTIL    (IDX         >   7  )
                                 OR  (IDY         >=  40 )
                       IF      SPA-GMN-RIHAYMD (IDX)  NOT =   SPACE
                           ADD     1               TO  IDY
                           MOVE    SPA-GMN-RIHAYMD (IDX)
                                                   TO  K02N-TABOO-ITEM
                                                               (IDY)
                       END-IF
                   END-PERFORM
      *            急性発症日
                   IF     (SPA-GMN-KYUSEI     NOT =   SPACE )
                      AND (IDY                    <   40    )
                       ADD     1                  TO  IDY
                       MOVE    SPA-GMN-KYUSEI     TO  K02N-TABOO-ITEM
                                                              (IDY)
                   END-IF
               WHEN    1   THRU   4
      *            患者情報
                   PERFORM  VARYING   IDX     FROM    1   BY  1
                            UNTIL    (IDX         >   8  )
                                 OR  (IDY         >=  40 )
                       IF      SPA-GMN-TABOO (IDY2 IDX)
                                                   NOT =   SPACE
                           ADD     1               TO  IDY
                           MOVE    SPA-GMN-TABOO (IDY2 IDX)
                                                   TO  K02N-TABOO-ITEM
                                                               (IDY)
                       END-IF
                   END-PERFORM
      *H28.10
               WHEN    6
      *            介護情報
                   IF      (IDY                <   40 )
                      AND  (SPA-GMN-YOUKAIGO   NOT =   SPACE)
                       ADD     1               TO  IDY
                       MOVE    SPA-GMN-YOUKAIGO
                                                   TO  K02N-TABOO-ITEM
                                                               (IDY)
                   END-IF
               END-EVALUATE
           END-PERFORM
           .
       5003-TABBOHEN-EXT.
           EXIT.
      *
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
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN-CUR         =   2
               IF      K02N-INPUTCD(SPA-MAP-IDX)   =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-GMN-CUR         =   1
               IF      WRK-IDX2            >   MAX-GMN
      *********    MOVE    09              TO  SPA-GMN-CUR
      *            最終行へ
                   MOVE    MAX-GMN         TO  WRK-IDX2
               END-IF
           END-IF
      *
      *    患者番号が空白の時、患者番号へ
           IF      K02N-PTNUM          =  SPACE
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    ROW が優先される為、クリア
           MOVE    ZERO                TO  K02N-PANDATABLE1-TROW
           MOVE    ZERO                TO  K02N-PANDATABLE1-TCOLUMN
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    09
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
      *            入力コード
      ****         MOVE    "INPUTCD"       TO  MCP-WIDGET
      ****         MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
                   MOVE    "PANDATABLE1"   TO  MCP-WIDGET
                   MOVE    4               TO  K02N-PANDATABLE1-TROWATTR
      *H26.4
      *            最終行は画面の最後へ
                   IF     (WRK-IDX2        >=  WRK-MAP-MAX  )
                       MOVE    1           TO  K02N-PANDATABLE1-TROWATTR
                   END-IF
      *
      *            前次頁の時は、画面の先頭へ
                   IF     (FLG-F6          =   1)  OR
                          (FLG-F7          =   1)
                       MOVE    ZERO            TO
                                               K02N-PANDATABLE1-TROWATTR
                       MOVE    ZERO            TO  FLG-F6
                       MOVE    ZERO            TO  FLG-F7
                   END-IF
      *
                   MOVE    2               TO  K02N-PANDATABLE1-TCOLUMN
                   MOVE    WRK-IDX2        TO  K02N-PANDATABLE1-TROW
      *
      *            エラー位置の背景色
                   IF     (SPA-ERRCD       =   SPACE )
                   OR     (SPA-ERRCD       =   "K999"
                                           OR  "9100"
                                           OR  "7001"
                                           OR  "7007"
                                           OR  "7004")
                   OR    ((WRK-MAP-CUR     =   ZERO  )   AND
                          (WRK-MAP-CUR2    =   ZERO  ))
                   OR     (K02N-INPUTCD (WRK-IDX2)  =   SPACE )
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD(1:1)  =   "K"
      *                    白の時、設定なし
                           IF      SPA-KEICOLORS   NOT =   CL-WHITE
                               MOVE    SPA-KEICOLORS
                                   TO  K02N-INPUTCD-BGCOLOR (WRK-IDX2) 
                           END-IF
                       ELSE
      *                    白の時、設定なし
                           IF      SPA-ERRCOLORS   NOT =   CL-WHITE
                               MOVE    SPA-ERRCOLORS
                                   TO  K02N-INPUTCD-BGCOLOR (WRK-IDX2) 
                           END-IF
                       END-IF
                   END-IF
      *
               WHEN    02
      *            名称
      ****         MOVE    "MEISYO"        TO  MCP-WIDGET
      ****         MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
                   MOVE    "PANDATABLE1"   TO  MCP-WIDGET
                   MOVE    4               TO  K02N-PANDATABLE1-TROWATTR
      *H26.4
      *            最終行は画面の最後へ
                   IF     (WRK-IDX2        >=  WRK-MAP-MAX  )
                       MOVE    1           TO  K02N-PANDATABLE1-TROWATTR
                   END-IF
      *
                   MOVE    3               TO  K02N-PANDATABLE1-TCOLUMN
                   MOVE    WRK-IDX2        TO  K02N-PANDATABLE1-TROW
               WHEN    03
                   MOVE    "KEI"           TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(4:2)
               WHEN    04
                   MOVE    "DOSELNUM"      TO  MCP-WIDGET
               WHEN    05
                   MOVE    "HKNCOMBI"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYMD"        TO  MCP-WIDGET
               WHEN    08
                   MOVE    "JIKANKBN"      TO  MCP-WIDGET
               WHEN    20
                   MOVE    "B11S"          TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B03"           TO  MCP-WIDGET
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
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *
           EVALUATE    SPA-ERRCD
               WHEN    "9999"
                   STRING  "他端末で使用中です。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9998"
                   STRING  "他端末で使用中です。"
                           "更新はできません。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9997"
                   STRING  "他端末で使用中です。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        エラー画面２
               PERFORM 5101-KERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           INITIALIZE                      KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KERR2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE
                   "すでに当日診察料を算定しています。"
                                       TO  WRK-KIDMSG(1:34)
                   MOVE
                   "ＯＫで同日再診へ振り替えます。"
                                       TO  WRK-KIDMSG(35:)
               WHEN    "0102"
                   MOVE    "外来管理加算は算定できません。削除します"
                                       TO  WRK-KIDMSG
               WHEN    "0103"
                   MOVE    "継続中です。患者設定画面へ遷移します。"
                                       TO  WRK-KIDMSG
               WHEN    "0104"
                   MOVE    "処理を終了します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0105"
                   STRING  "保険組合せが変更されました。"
                           "よろしいですか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0124"
                   STRING  "保険組合せが変更されました。"
                           "同日の同じ受診履歴に追加されます。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0106"
                   MOVE    "同日再診が３回以上になります。"
                                       TO  WRK-KIDMSG
                   MOVE    "訂正のみ行うようにして下さい。"
                                       TO  WRK-KIDMSG(31:)
               WHEN    "0107"
                   STRING   "診療科が変更されました。"
                            "現在の診療内容を残しますか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0123"
                   STRING   "診療科が変更されました。"
                            "よろしいですか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0128"
                   STRING  "診療科が変更されました。"
                           "同日の同じ受診履歴に追加されます。"
                           "よろしいですか？"
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0122"
                   STRING  "この科の中途終了データが存在します。"
                           "ＯＫで中途終了内容へ置換えます。"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0108"
               MOVE
               "初診算定日を登録します。現在の診療内容はクリアします。"
                                       TO  WRK-KIDMSG
               WHEN    "0109"
                   MOVE
                   "既に診察料を他科で算定しています。"
                                       TO  WRK-KIDMSG(1:34)
                   MOVE
                   "ＯＫで同日再診料へ変更します。"
                                       TO  WRK-KIDMSG(35:)
               WHEN    "0110"
               MOVE
                   "初診料算定へ変更します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0111"
                   MOVE
                   "診療日が未来日です。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0112"
                   MOVE    "初診料算定へ変更しますが、"
                                       TO  WRK-KIDMSG
                   MOVE    "転帰が未入力の病名が存在します。"
                                       TO  WRK-KIDMSG(27:)
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(59:)
               WHEN    "0113"
                   MOVE    "特定疾患処方管理加算が算定できます。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで自動算定します。"
                                       TO  WRK-KIDMSG(37:)
               WHEN    "0114"
                   MOVE    "厚生労働大臣が定める患者の再診４回目以降へ"
                                       TO  WRK-KIDMSG
                   MOVE    "振替えます。よろしいですか？"
                                       TO  WRK-KIDMSG(43:)
               WHEN    "0115"
                   MOVE    "人工腎臓の再診４回目以降へ振替えます。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(39:)
               WHEN    "0116"
                   STRING  "診療行為内容をクリアします。"
                           "よろしいですか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0127"
                   STRING  "ＯＫで展開中の中途終了データを削除します。"
                           "よろしいですか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0118"
                   STRING  "この保険の中途終了データが存在します。"
                           "ＯＫで中途終了内容へ置換えます。"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0119"
                   MOVE    "既に他保険で受診済みです。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで他保険にて算定済へ置換えます"
                                       TO  WRK-KIDMSG(27:)
               WHEN    "0117"
                   MOVE    "中途終了します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0120"
                   STRING  "診療日では退院済です。"
                           "外来の画面に遷移しますか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0160"
                   STRING  "包括検査で同じ検査があります。"
                           "ＯＫですべての同一検査を削除します。"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0180"
                   STRING  "個別療法が１０単位を超えます。"
                           "逓減しますか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0125"
                   STRING  "患者取消が選択されました。"
                           "表示内容をクリアします。よろしいですか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0126"
                   STRING  "アフターケアは外来での入力になります。"
                           "外来へ遷移しますか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0150"
                   STRING  "包括算定エラーの項目があります。"
                           "エラー項目を削除します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *VER.4.6
               WHEN    "0138"
                   STRING   "診療科の保険組合せに変更しますか？"
                            "【"
                                       DELIMITED   BY  SIZE
                             HZN-SPA-GMN-HKNCOMBI-G
                                       DELIMITED   BY  "  "
                             "】"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *ver.4.7
               WHEN    "0200"
                   STRING   "入院期間を変更します。"
                            "ＯＫで変更後に中途終了がなければ"
                            "現在の診療内容を残します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
           EVALUATE    SPA-KIDCD
               WHEN    "0109"
               WHEN    "0118"
               WHEN    "0119"
               WHEN    "0107"
               WHEN    "0180"
               WHEN    "0127"
               WHEN    "0138"
               WHEN    "0200"
                   MOVE    "ＮＯ"            TO  KID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       530-KID2SET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD2
               WHEN    "0117"
                   STRING  "登録後、帳票を発行する時は各帳票の発行を、"
                           "発行しない時は登録を押下して下さい。"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD2
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID2
           INITIALIZE                      KID2
           MOVE    SPA-KIDCD2          TO  KID2-ID2CODE
           MOVE    WRK-KIDMSG          TO  KID2-ID2MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-KID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
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
       930-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ次読込
      *****************************************************************
       970-WKSRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WKSRYACT-REC
               MOVE    ZERO                TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                      WKSRYACT-REC
               MOVE    1                   TO  FLG-WKSRYACT
           END-IF
      *
           .
       970-WKSRYACT-READ-EXT.
           EXIT.
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
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      **   EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
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
      *    ＤＢＣＬＯＳＥ処理
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
      *TEST
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       910-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       990-SPATMP-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *TEST
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *    排他中
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN2-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *    排他中
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
           END-IF
           .
       990-LOCK-IN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
