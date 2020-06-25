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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHC02V01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 処方箋　印刷（Ａ５）
      *                      （ORCHC02Q の QR印刷を削除）
      *                      平成２４年３月末まで使用
      *                      （Ｈ２８年４月改正でCALLＰＧ作成対応）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/09/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/10/07  在宅薬剤・器材の院外用追加
      * 01.00.02     NACL-多々納  02/10/25  入院対応
      * 01.00.03     NACL-多々納  02/11/01  受給者番号の全角８文字以上対応
      * 01.00.04     NACL-多々納  02/12/29  高８、高９編集修正
      * 01.00.05     NACL-多々納  03/01/20  その他器材の編集追加
      *                                     中途の在宅編集修正
      * 01.00.06     NACL-多々納  03/03/06  剤明細１９以上の編集修正
      *                                     全角変換時空白判定追加
      * 01.00.07     NACL-多々納  03/04/17  免除判定削除
      * 01.00.08     NACL-多々納  03/06/18  用法２０文字以上編集修正
      * 01.00.09     NACL-多々納  03/06/27  出力先プリンタサブ追加
      * 01.00.10     NACL-多々納  03/12/16  院内処方（213,223,233)を
      *                                     院内処方せんのみの印刷とする
      * 01.00.11     NACL-多々納  04/03/08  在宅の対象判定修正
      * 01.00.12     NACL-多々納  04/05/27  ２０行目の余白に編集追加
      * 01.00.13     NACL-多々納  04/07/20  複数科・保険同時入力に対応
      * 01.00.14     NACL-多々納  05/05/11  備考コメント追加
      *                                     備考欄を４２文字へ変更
      * 01.00.15     NACL-多々納  05/10/26  MONFUNCへ変更
      * 01.00.16     NACL-多々納  05/11/09  再印刷処理追加
      * 01.00.17     NACL-多々納  06/01/24  用量コード判定追加
      * 01.00.18     NACL-多々納  06/03/XX  Ｈ１８年４月改正対応
      * 01.00.19     NACL-多々納  06/06/09  後発医薬品区分変更
      * 03.02.00     NACL-多々納  06/09/20  Ｈ１８年１０月改正対応
      * 03.04.00     NACL-多々納  06/12/20  内用総量編集対応
      *                                     一般名記載対応
      * 03.04.01     NACL-森脇    07/01/17  医療機関名称、住所セット変更
      * 03.04.02     NACL-多々納  07/02/28  包括分保険同時入力対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 03.05.00     NACL-多々納  07/05/22  再印刷処理追加
      * 04.00.00     NACL-多々納  07/09/21  Ａ４印刷として新規作成
      *                                     ＱＲ印刷対応
      * 04.01.00     NACL-多々納  07/11/22  公害保険表示
      *                                     器材商品名コード対応
      * 04.01.00     NACL-多々納  08/01/20  編集処理変更
      *                                     パラメタによる編集対応
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      * 04.03.00     NACL-多々納  08/08/28  退院時院外処方せん対応
      * 04.05.00     NACL-多々納  09/06/XX  数量小数５桁対応
      * 04.05.00     NACL-多々納  09/09/XX  '0086'コメントコード対応他
      * 04.05.01     NACL-多々納  10/03/02  Ｈ２２年４月改正対応
      * 04.06.00     NACL-多々納  10/06/XX  受給者番号１５文字対応
      * 04.06.00     NACL-多々納  10/07/27  患者コメント等備考編集対応
      * 04.06.00     NACL-多々納  10/08/26  高齢非該当対応追加
      * 04.06.00     NACL-多々納  11/02/21  用法コードコメント対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HC02.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-GENERICNAME     PIC 9(01).
      *
           03  FLG-WKSRYEND            PIC 9(01).
      *    院内・院外処方区分
           03  FLG-SYOHO-IN            PIC 9(01).
      *    剤毎の区分
           03  FLG-KAISU               PIC 9(01).
           03  FLG-ZAITAKU-ARI         PIC 9(01).
           03  FLG-SOURYOHEN           PIC 9(01).
           03  FLG-HENSYU              PIC 9(01).
      *    公費対象
           03  FLG-KOH-NO              PIC 9(01).
      *
           03  FLG-SP                  PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      *    処方せん料有無
           03  FLG-SYOHOU              PIC 9(01).
      *    印刷有無
           03  FLG-PRINT               PIC 9(01).
      *    麻薬区分
           03  FLG-MAYAKU              PIC 9(01).
      *    老人フラグ
           03  FLG-RJNHKN              PIC 9(01).
      *    対象外フラグ
           03  FLG-TAIGAI              PIC 9(01).
           03  FLG-OK2                 PIC 9(01).
      *
      *    後発医薬品変更可
           03  FLG-KOUHATU-HEN         PIC 9(01).
           03  FLG-KOUHATU-CHK         PIC 9(01).
           03  FLG-KOUHATU             PIC 9(01).
      *    後発医薬品あり
           03  FLG-KOUHATU-1           PIC 9(01).
           03  FLG-KOUHATU-1-CHK       PIC 9(01).
      *    後発医薬品変更不可
           03  FLG-KOUHATU-FUKA        PIC 9(01).
      *    公費編集
           03  FLG-KOHERR              PIC 9(01).
      *
      *---(04.02.00) LINE ADD START ------------------------------------
      *    後発医薬品変更不可あり
           03  FLG-KOUHATUFUKA-IN      PIC 9(01).
           03  FLG-KOUHATUFUKA-ARI     PIC 9(01).
      *    後発品・先発品あり
           03  FLG-KOUHATU-ARI         PIC 9(01).
      *---(04.02.00) LINE ADD END   ------------------------------------
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXA2               PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
           03  IDX-B               PIC 9(04).
           03  IDX-B1              PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
      *
           03  KETA                PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-SRY             PIC 9(04).
           03  IDXB                PIC 9(04).
           03  IDX-L               PIC 9(04).
           03  IDX-L2              PIC 9(04).
      *---(04.00.00) LINE ADD START ------------------------------------
      *ＱＲ対応
           03  IDX-QR1                 PIC 9(04).
           03  IDX-QR2                 PIC 9(04).
           03  IDX-QRB                 PIC 9(04).
           03  IDX-QRB2                PIC 9(04).
           03  IDQ                     PIC 9(04).
      *---(04.00.00) LINE ADD END   ------------------------------------
           03  IDXF                    PIC 9(04).
           03  IDXF2                   PIC 9(04).
           03  IDXF3                   PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PAGE            PIC 9(03).
      *
      *    保険組合せ
           03  WRK-HKNCOMBINUM     PIC  X(04).
           03  WRK-SRYKA           PIC  X(02).
           03  WRK-DRCD            PIC  X(05).
           03  WRK-DENPNUM         PIC  9(07).
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-HKNJA           PIC X(08).
           03  WRK-HKNJA-G         PIC X(16).
      *    公費情報
           03  WRK-FTNSNO          PIC X(16).
           03  WRK-JKYSNO          PIC X(40).
           03  WRK-JKYSNO-MAX      PIC 9(04).
           03  WRK-KOH-FTNJA-G.
               05  WRK-KOH-FTNJA-OCC       OCCURS   2.
                   07  WRK-HEN-FTNJANUM    PIC X(16).
                   07  WRK-HEN-JKYSNUM     PIC X(40).
                   07  WRK-JKYSNUM-MAX     PIC 9(04).
      *
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-SURYO-N22       PIC X(22).
           03  WRK-SURYO-N         PIC X(18).
      *    数量小数点以下５桁
           03  WRK-SURYO2           PIC 9(05)V9(05).
           03  WRK-SURYO2-R         REDEFINES   WRK-SURYO2.
               05  WRK-SURYO2-S1    PIC 9(05).
               05  WRK-SURYO2-S2    PIC 9(05).
           03  WRK-SURYO-X2.
               05  WRK-SURYO2-Z1   PIC ZZZZ9.
               05  WRK-SURYO2-Z2   PIC X(01).
               05  WRK-SURYO2-Z3   PIC ZZZZZ.
           03  WRK-SURYO-X2-R      REDEFINES   WRK-SURYO-X2.
               05  WRK-SURYO2-X21  PIC X(02).
               05  WRK-SURYO2-X29  PIC X(09).
           03  WRK-KANZANCHI       PIC 9(05)V9(5).
      *
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
      *    名称
           03  WRK-COM-NAME        PIC X(200).
           03  WRK-TNS-NAME        PIC X(200).
           03  WRK-NAME            PIC X(200).
      *    用法コメント名称
           03  WRK-YOH-NAME        PIC X(200).
      *    剤内容
           03  WRK-KAISU           PIC 9(05).
           03  WRK-ZAITENKEI       PIC 9(07).
           03  WRK-YKZTEN          PIC 9(07).
           03  WRK-KIZAITEN        PIC 9(07).
      *
      *    文字変換
           03  WRK-ZENKAKU-G           PIC X(100).
           03  WRK-MOJI                PIC X(100).
           03  WRK-MOJISU              PIC 9(02).
      *    記号番号
           03  WRK-KIGONUM             PIC X(80).
           03  WRK-NUM                 PIC X(80).
           03  WRK-HEN-KIGONUM         PIC X(80).
      *
      *---(04.05.01) LINE ADD START ------------------------------------
      *    医療機関情報（全角）
           03  WRK-PREFNUM-J           PIC X(04).
           03  WRK-TENHYOKBN-J         PIC X(02).
           03  WRK-HOSPCD-J            PIC X(14).
      *パラメタによる編集
      *    地方公費
           03  WRK-HEN-KOH-G.
               05  WRK-HEN-KOH         PIC X(86)
                                       OCCURS   4.
      *    保険医名称
           03  WRK-DRNAME              PIC X(50).
      *    交付年月日
           03  WRK-KOFYMD              PIC X(22).
      *    負担割合
           03  WRK-HEN-FTNRATE         PIC X(03).
           03  WRK-HEN-FTNWARI         PIC X(02).
           03  WRK-FTNRATE             PIC 9(02)V9.
           03  WRK-FTNRATE-R           REDEFINES   WRK-FTNRATE.
               05  WRK-FTNRATE-S1      PIC 9(02).
               05  WRK-FTNRATE-S2      PIC 9(01).
           03  WRK-FTN-ZZ9             PIC ZZ9.
           03  WRK-FTN-Z92             PIC 9.9.
      *    診療科名
           03  WRK-HEN-SRYKANAME       PIC X(40).
      *    ページ編集
           03  WRK-HEN-PAGE            PIC X(05).
           03  WRK-HEN-PAGE1           PIC Z9.
           03  WRK-HEN-PAGE2           PIC Z9.
      *
      *    麻薬編集
           03  WRK-HENSYU-AREA.
               05  WRK-HENSYU          PIC X(86)   OCCURS  3.
           03  IDX-H                   PIC 9(04).
           03  WRK-ADRS                PIC X(200).
      *    備考負担割合
           03  WRK-BIKOU               PIC X(86).
      *    医療機関名称編集
           03  WRK-HOSPNAME            PIC X(80).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(40).
               05  WRK-HOSPNAME2       PIC X(40).
           03  WRK-HOSPADRS            PIC X(80).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(40).
               05  WRK-HOSPADRS2       PIC X(40).
           03  WRK-HOSPTEL             PIC X(15).
      *
      *ＱＲ対応
           03  WRK-FILE-CNT            PIC 9(02).
           03  WRK-FILE-CNT2           PIC 9(02).
           03  WRK-SYS-TIME            PIC 9(08).
      *
           03  WRK-BIKOU-FTN           PIC X(01).
      *
      *    剤毎の内容
       01  WRK-NAIYOU-AREA.
           03  WRK-NAIYOU-TBL          OCCURS   100.
               05  WRK-NAIYOU-Z            PIC X(86).
               05  WRK-NAIYOU-R1           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-NO              PIC X(04).
                   07  WRK-K2              PIC X(02).
                   07  WRK-MEISYOU1        PIC X(44).
                   07  WRK-SURYOU          PIC X(18).
                   07  WRK-TANI            PIC X(12).
                   07  WRK-FIL11           PIC X(06).
               05  WRK-NAIYOU-R2           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL21           PIC X(06).
                   07  WRK-MEISYOU2        PIC X(66).
                   07  WRK-K3              PIC X(02).
                   07  WRK-KAISUU          PIC X(06).
                   07  WRK-HINICHI         PIC X(06).
               05  WRK-NAIYOU-R3           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL23           PIC X(06).
                   07  WRK-MEISYOU3        PIC X(80).
               05  WRK-NAIYOU-R4           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL41           PIC X(46).
                   07  WRK-SURYOU4         PIC X(22).
                   07  WRK-TANI4           PIC X(12).
                   07  WRK-FIL42           PIC X(06).
      *    薬剤名編集
       01  WRK-MEISAI-AREA.
           03  WRK-M-NAME              PIC X(100).
           03  WRK-M-KETA              PIC 9(03).
           03  WRK-FUKA-MEI-AREA.
      *        変更不可コード編集
               05  WRK-FUKA-TBL            OCCURS   3.
                   07  WRK-KOU-NAME            PIC X(100).
                   07  WRK-KOU-TNSNAME         PIC X(100).
                   07  WRK-KOU-KETA            PIC 9(03).
           03  WRK-HEN-NAME            PIC X(100).
           03  WRK-HEN-NAME2           PIC X(100).
           03  WRK-HEN-KETA            PIC 9(03).
      *
      *    剤内容テーブル
       01  WRK-SRYACT-GRP-AREA.
           03  WRK-SRYACT-TBL          OCCURS   50.
      *診療コード
               05  WRK-SRY-SRYCD           PIC  X(09).
      *数量
               05  WRK-SRY-SRYSURYO        PIC  9(05)V9(05).
      *回数
               05  WRK-SRY-SRYKAISU        PIC  9(03).
      *明細請求区分
               05  WRK-SRY-MEISKYFLG       PIC  X(01).
      *自動算定区分
               05  WRK-SRY-AUTOKBN         PIC  X(01).
      *名称入力番号
               05  WRK-SRY-INPUTNUM        PIC  9(03).
      *自費金額
               05  WRK-SRY-JIHIMONEY       PIC  9(07).
      *名称
               05  WRK-SRY-COM-NAME        PIC X(200).
      *
           03  WRK-SRY-MAX                 PIC 9(03).
      *
      *    処方内容
       01  WRK-SYOHOU-AREA.
           03  WRK-SYOHOU-PAGE         OCCURS   10.
               05  WRK-SYOHOU-TBL          OCCURS   20.
                   07  WRK-NAIYOU          PIC X(86).
           03  WRK-PAGE-MAX                PIC 9(03).
      *
      *    処方せん明細行
       01  LINE-MAX                PIC 9(03)  VALUE 20.
      *    備考行
       01  BIKOU-MAX               PIC 9(02)  VALUE 9.
       01  BIKOU-MAX2              PIC 9(02)  VALUE 7.
      *
      *    コメント備考
       01  WRK-BIKOU-AREA.
           03  WRK-BIKOU-G.
               05  WRK-BIKOU-NAME      PIC X(172)
                                       OCCURS   9.
           03  WRK-BIKOU-G2            REDEFINES
                                       WRK-BIKOU-G.
               05  WRK-BIKOU-NAME2     PIC X(86)
                                       OCCURS   18.
           03  IDX-BIKOU                   PIC 9(04).
      *    患者コメント
           03  WRK-COMMENT-HEN         PIC X(172).
           03  WRK-PTCOMMENT-G.
               05  WRK-PTCOMMENT       PIC X(172)
                                       OCCURS   2.
           03  WRK-PTCOMMENT-HEN           PIC X(86).
           03  IDX-COM                     PIC 9(04).
           03  IDX-COM2                    PIC 9(04).
           03  COM-MAX                     PIC 9(04).
      *
      *    ＱＲ位置
      *01  QR-FILE-NAME.
      *    03  FILLER              PIC X(05)   VALUE   "/tmp/".
      *    03  QR-FILE-HOSPNUM     PIC 9(02)   VALUE   ZERO.
      *    03  QR-FILE-FORM-ID     PIC X(04)   VALUE   SPACE.
      *    03  FILLER              PIC X(01)   VALUE   ".".
      *    03  QR-FILE-TERMID      PIC X(32)   VALUE   SPACE.
      *    03  FILLER              PIC X(01)   VALUE   ".".
      *    03  QR-FILE-SYOHMS      PIC 9(08)   VALUE   ZERO.
      *    03  FILLER              PIC X(04)   VALUE   ".qr_".
      *    03  QR-FILE-ID          PIC 9(02)   VALUE   ZERO.
      *    03  FILLER              PIC X(04)   VALUE   ".png".
      *
      *01  KUHAKU-FILE-NAME        PIC X(512).
      *    空白の画像
      *01  KUHAKU-NAME.
      *    03  FILLER          PIC X(05)   VALUE   "data/".
      **** 03  FILLER          PIC X(15)   VALUE   "transparent.jpg".
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-PAGE               PIC X(01).
           03  PARA-FTNKBN             PIC X(01).
           03  PARA-KOHKBN             PIC X(01).
           03  PARA-SRYKAKBN           PIC X(01).
           03  PARA-DRKBN              PIC X(01).
           03  PARA-KOFYMDKBN          PIC X(01).
           03  PARA-TABOO              PIC X(01).
           03  PARA-ALLERGY            PIC X(01).
           03  PARA-KANSENSYO          PIC X(01).
           03  PARA-COMMENT            PIC X(01).
      *H24.3
           03  PARA-GE201203           PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *    処方箋ＧＥ表示
           COPY    "CPSK1030.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *---(04.03.00) LINE ADD START ------------------------------------
      *    出力先プリンタ名割り当て情報（入院）
           COPY  "CPSK1034.INC".
      *---(04.03.00) LINE ADD END   ------------------------------------
      *
      *---(03.04.01) LINE ADD START -----------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(03.04.01) LINE ADD END   -----------------------------------
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   100.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
           03  LNK-WKSRY-SYORI         PIC X(01).
      *
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS   10.
           COPY    "CPSRYACT.INC"     REPLACING
                                      //SRY-// BY //TBL-SRY-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   免除判定サブ
      **** COPY    "CPORCSMENJYO.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *ＱＲ対応
      *    ＱＲデータ作成サブパラメタ
           COPY    "CPORCSQRCSV.INC".
      *    ファイルデイレクトリ位置指定サブ
      *    COPY    "CPMKPASS.INC".
      *    ファイル存在チェック
      ***  COPY    "CPORCSFILECHK.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCHC19.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC19AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-NAIYOU-AREA
           INITIALIZE                  WRK-SYOHOU-AREA
           INITIALIZE                  WRK-BIKOU-AREA
      *
      *---(04.03.00) LINE UPD START ------------------------------------
      **** ACCEPT  SYS-TIME            FROM    TIME
      *    ファイル名編集処理
           PERFORM 1001-HC02-FILE-SEC
      *---(04.03.00) LINE UPD END   ------------------------------------
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *    処方内容編集
           PERFORM 200-MAIN-SYORI-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *    ページ表示
           MOVE    "PAGE"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-PAGE
      *    負担割合
           MOVE    "FTNKBN"            TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-FTNKBN
      *    地方公費非表示
           MOVE    "KOHKBN"            TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-KOHKBN
      *    診療科表示
           MOVE    "SRYKA"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SRYKAKBN
      *    ドクター
           MOVE    "DRKBN"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-DRKBN
      *    交付年月日
           MOVE    "KOFYMDKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-KOFYMDKBN
      *    備考禁忌区分
           MOVE    "TABOO"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-TABOO
      *    備考アレルギー区分
           MOVE    "ALLERGY"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-ALLERGY
      *    備考感染症区分
           MOVE    "KANSENSYO"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-KANSENSYO
      *    患者コメント
           MOVE    "COMMENT"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-COMMENT
      *H24.3
      *    一般名記載なし区分
           MOVE    "GE201203"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-GE201203
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC02"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC02"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    WRK-SYS-TIME        TO  HC01PARA-SYOHMS
      *
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT               PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *---(04.03.00) LINE ADD START ------------------------------------
      *****************************************************************
      *    ファイル名処理
      *****************************************************************
       1001-HC02-FILE-SEC            SECTION.
      *
           MOVE    1                   TO  WRK-PAGE
           MOVE    1                   TO  WRK-FILE-CNT2
           MOVE    ZERO                TO  FLG-OK
           PERFORM
               UNTIL       FLG-OK      =   1
               MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
               MOVE    "HC02"              TO  HC01PARA-FORM-ID
               MOVE    SPA-TERMID          TO  HC01PARA-TERMID
               MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
               ACCEPT  SYS-TIME            FROM    TIME
               COMPUTE WRK-SYS-TIME        =   SYS-TIME
                                           +   WRK-FILE-CNT2
               MOVE    WRK-SYS-TIME        TO  HC01PARA-SYOHMS
      *v4.8
      *        一時ファイル名取得
               INITIALIZE                      SGETTEMP-AREA
               MOVE    HC01PARA            TO  SGETTEMP-BASENAME
               CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
               OPEN    INPUT               PRT-FILE
               IF      STS-PARA        NOT =   ZERO
                   MOVE    1               TO  FLG-OK
               END-IF
               CLOSE                       PRT-FILE
           END-PERFORM
           MOVE    ZERO                TO  WRK-PAGE
           MOVE    ZERO                TO  WRK-FILE-CNT2
      *
           .
       1001-HC02-FILE-EXT.
           EXIT.
      *---(04.03.00) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *     印刷用データ出力
            WRITE  HC02
      *
            CLOSE                      PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
      *     平成２２年４月からレイアウト変更
            IF     SPA-SRYYMD          <   "20100401"
                MOVE   "HC02.red"          TO  MKPRT-DIA
            ELSE
                MOVE   "HC02V01.red"       TO  MKPRT-DIA
            END-IF
            MOVE   SPACE               TO  MKPRT-DEF
      *v4.8
      ****  MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(2)   TO  MKPRT-PRTNM
      *---(04.03.00) LINE ADD START ------------------------------------
      *    退院時院外処方せん（入院）
            IF    (ORCHC19-KBN         =   "N"       )  AND
                  (SYS-1034-PRTNM(12)  NOT =   SPACE )
                MOVE   SYS-1034-PRTNM(12)  TO  MKPRT-PRTNM
            END-IF
      *---(04.03.00) LINE ADD START ------------------------------------
      *
            MOVE   "処方せん"          TO  MKPRT-INFO
            CALL   "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
      *    各ＤＢ検索
           PERFORM 4001-DB-KENSAKU-SEC
      *    ユーザ設定編集
           PERFORM 4002-USER-SETHEN-SEC
      *    麻薬編集
           IF      FLG-MAYAKU          =   1
               PERFORM 2003-MAYAKU-HEN-SEC
           END-IF
      *ＱＲ対応
      *    ＱＲデータ出力処理（日数ゼロは対象外）
      *    IF      (ORCHC19-KBN        =   SPACE
      *                                OR  "3"
      *                                OR  "4"
      *                                OR  "5"
      *                                OR  "A"
      *---(04.03.00) LINE ADD START ------------------------------------
      *                                OR  "N"   )
      *---(04.03.00) LINE ADD END   ------------------------------------
      *        AND (IDX-QR1            >   ZERO  )
      *        医師・交付日未設定は作成なし
      *        IF      (WRK-DRCD           =   SPACE )
      *            OR  (WRK-DRNAME         =   SPACE )
      *            OR  (WRK-KOFYMD         =   SPACE )
      *            INITIALIZE                  ORCSQRCSVAREA
      *        ELSE
      *            PERFORM 600-QRDATA-SYORI-SEC
      *        END-IF
      *    END-IF
      *****MOVE    ZERO                TO  WRK-FILE-CNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HC02-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票各ＤＢ検索処理
      *****************************************************************
       4001-DB-KENSAKU-SEC                SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
      *    患者保険情報検索
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
      *    主保険−ＩＤ
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 920-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF      NOT =   ZERO
               INITIALIZE              PTHKNINF-REC
           END-IF
      *
      *    保険者番号変換処理
           MOVE    SPACE               TO  WRK-HKNJA
           MOVE    9                   TO  IDY
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    8   BY  -1
                   UNTIL       IDX     <   1
               IF      PTHKN-HKNJANUM(IDX:1)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      FLG-OK              =   1
                   COMPUTE IDY         =   IDY     -   1
                   MOVE    PTHKN-HKNJANUM(IDX:1)   TO  WRK-HKNJA (IDY:1)
               END-IF
           END-PERFORM
           MOVE    WRK-HKNJA           TO  WRK-MOJI
           MOVE    8                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:16) TO  WRK-HKNJA-G
      *
      *    保険番号マスター検索
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM      NOT =   ZERO
               INITIALIZE              HKNNUM-REC
           END-IF
      *
      *    公費情報
           MOVE    ZERO                TO  IDX-K2
           INITIALIZE                  WRK-KOH-FTNJA-G
           MOVE    ZERO                TO  IDX-K3
           INITIALIZE                  WRK-HEN-KOH-G
           PERFORM VARYING    IDX-K    FROM    1   BY  1
                   UNTIL     (IDX-K    >   3   )
                         OR  (IDX-K2   >=  2   )
                         OR  (COMB-KOHID (IDX-K)   =   ZERO )
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    COMB-KOHID (IDX-K)  TO  PTKOH-KOHID
               PERFORM 940-PTKOHINF-READ-SEC
               IF      FLG-PTKOHINF        =   ZERO
                   PERFORM 40011-PTKOHINF-HEN-SEC
                   IF      FLG-KOH-NO           =   ZERO
                        ADD     1               TO  IDX-K2
                        MOVE    WRK-FTNSNO      TO  WRK-HEN-FTNJANUM
                                                           (IDX-K2)
                        MOVE    WRK-JKYSNO      TO  WRK-HEN-JKYSNUM
                                                           (IDX-K2)
                        MOVE    WRK-JKYSNO-MAX  TO  WRK-JKYSNUM-MAX
                                                           (IDX-K2)
                   END-IF
               ELSE
                   ADD     1               TO  IDX-K2
               END-IF
           END-PERFORM
      *
      *    記号・番号
      *    全角変換
           IF      PTHKN-KIGO          =   SPACE
               MOVE    SPACE               TO  WRK-KIGONUM
           ELSE
               MOVE    PTHKN-KIGO          TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                           TO  WRK-KIGONUM
           END-IF
           IF      PTHKN-NUM           =   SPACE
               MOVE    SPACE               TO  WRK-NUM
           ELSE
               MOVE    PTHKN-NUM           TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                           TO  WRK-NUM
           END-IF
      *
           MOVE    SPACE               TO  WRK-HEN-KIGONUM
           STRING  WRK-KIGONUM         DELIMITED   BY  SPACE
                   "・"                DELIMITED   BY  SIZE
                   WRK-NUM             DELIMITED   BY  SPACE
                   INTO                    WRK-HEN-KIGONUM
           END-STRING
      *
      *    公害保険は保険名を保険者番号へ
           IF      COMB-HKNNUM         =  "975"
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNJA-G
           END-IF
      *
      *    労災の時、保険者番号・記号の変更
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-PTRSIINF        =   ZERO
                   MOVE    SPACE               TO  WRK-HEN-KIGONUM
                   MOVE    SPACE               TO  WRK-HKNJA-G
      *            保険者番号へ記号（労災区分名）
                   MOVE    WRK-KIGONUM         TO  WRK-HKNJA-G
      *            記号へ患者交付番号
                   IF      PTRSI-KOFUNUM       =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    PTRSI-KOFUNUM       TO  WRK-MOJI
                       PERFORM 2009-HENKAN-SEC
                       MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                                   TO  WRK-HEN-KIGONUM
                   END-IF
               END-IF
           END-IF
      *
      *    ３歳未満・老人判定
           MOVE    SPACE               TO  WRK-BIKOU
           MOVE    SPACE               TO  WRK-BIKOU-FTN
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    WRK-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *---(04.02.00) LINE UPD START ------------------------------------
           IF      SPA-SRYYMD          <   "20080401"
      *        ３歳未満
               IF      AGECHK-NENREI       <   03
                   MOVE    "（３歳）"          TO  WRK-BIKOU
                   MOVE    "3"                 TO  WRK-BIKOU-FTN
               END-IF
           ELSE
      *        就学前
               IF      AGECHK-SYUGAKUKBN   =   1
                   MOVE    "（６歳）"
                                               TO  WRK-BIKOU
                   MOVE    "3"                 TO  WRK-BIKOU-FTN
               END-IF
           END-IF
      *---(04.02.00) LINE UPD END   ------------------------------------
      *    ７０歳以上
           MOVE    ZERO                TO  FLG-RJNHKN
           IF      AGECHK-NENREI       >=  70
      *        老人保険
               EVALUATE    COMB-HKNNUM
                   WHEN    "060"
      *                国保
                       EVALUATE    COMB-HOJOKBN
                           WHEN    "1"
                               MOVE    1           TO  FLG-RJNHKN
                           WHEN    "2"
                               MOVE    2           TO  FLG-RJNHKN
                           WHEN    "3"
                               MOVE    3           TO  FLG-RJNHKN
                       END-EVALUATE
      *                高齢非該当の判定
                       IF      AGECHK-ROUJIN       =   0
                           PERFORM 40015-PTKOHINF-976CHK-SEC
                       END-IF
      *                以外
                   WHEN    OTHER
      *                自費以外
                       IF      COMB-HKNNUM (1:2)   =   "98"
                           CONTINUE
                       ELSE
                           EVALUATE    COMB-HOJOKBN
                               WHEN    "9"
                               WHEN    "A"
                               WHEN    "B"
                               WHEN    "C"
      *                        １割
                                   MOVE    1           TO  FLG-RJNHKN
                               WHEN    "8"
                               WHEN    "D"
                               WHEN    "E"
                               WHEN    "F"
      *                        ２割
                                   MOVE    2           TO  FLG-RJNHKN
                               WHEN    "7"
                               WHEN    "G"
                               WHEN    "H"
                               WHEN    "I"
                                   MOVE    3           TO  FLG-RJNHKN
                           END-EVALUATE
                       END-IF
               END-EVALUATE
           END-IF
      *---(04.02.00) LINE ADD START ------------------------------------
      *    後期高齢者
           EVALUATE    COMB-HKNNUM
               WHEN    "039"
               WHEN    "040"
                   EVALUATE    COMB-HOJOKBN
                       WHEN    "1"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "3"
                           MOVE    3           TO  FLG-RJNHKN
                   END-EVALUATE
           END-EVALUATE
      *---(04.02.00) LINE ADD END   ------------------------------------
      *    老人公費（保険より公費の割合を優先する）
           PERFORM VARYING    IDX-K    FROM    1   BY  1
                   UNTIL      IDX-K    >   3
               IF      COMB-KOHHKNNUM (IDX-K)  =   "027"
                   EVALUATE    COMB-KOHPAYKBN (IDX-K)
                       WHEN    "01"
      *                        １割
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
      *                        ２割
                           MOVE    2           TO  FLG-RJNHKN
                       WHEN    "03"
      *                        ３割
                           MOVE    3           TO  FLG-RJNHKN
                   END-EVALUATE
                   MOVE    3               TO  IDX-K
               END-IF
           END-PERFORM
      *
           EVALUATE    FLG-RJNHKN
               WHEN    1
                   IF      SPA-SRYYMD          <   "20080401"
                       MOVE    "（高９）"          TO  WRK-BIKOU
                   ELSE
      *                Ｈ２０．４ から変更
                       MOVE    "（高一）"          TO  WRK-BIKOU
                   END-IF
                   MOVE    "1"                 TO  WRK-BIKOU-FTN
               WHEN    2
                   MOVE    "（高８）"          TO  WRK-BIKOU
                   MOVE    "2"                 TO  WRK-BIKOU-FTN
               WHEN    3
                   MOVE    "（高７）"          TO  WRK-BIKOU
                   MOVE    "2"                 TO  WRK-BIKOU-FTN
           END-EVALUATE
      *
           .
       4001-DB-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       40011-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH-NO
           MOVE    SPACE               TO  WRK-FTNSNO
           MOVE    SPACE               TO  WRK-JKYSNO
           MOVE    ZERO                TO  WRK-JKYSNO-MAX
      *    保険者番号
           IF      PTKOH-FTNJANUM      =   SPACE
               MOVE    SPACE           TO  WRK-FTNSNO
           ELSE
               MOVE    PTKOH-FTNJANUM      TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                           TO  WRK-FTNSNO
           END-IF
      *    受給者番号
           IF      PTKOH-JKYSNUM       =   SPACE
               MOVE    SPACE               TO  WRK-JKYSNO
           ELSE
               MOVE    PTKOH-JKYSNUM       TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    SPACE               TO  WRK-JKYSNO
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                            TO  WRK-JKYSNO
               MOVE    KANACHK-MAX          TO  WRK-JKYSNO-MAX
           END-IF
      *@@@ 埼玉県対応
      *    負担者番号編集なし
           IF     SYS-1001-PREFNUM     =   "11"
               IF      COMB-KOHHKNNUM (IDX-K)
                                           =   "342"
                                           OR  "343"
                                           OR  "344"
                   MOVE    SPACE           TO  WRK-FTNSNO
               END-IF
           END-IF
      *@@@ 埼玉県対応
      *
      *    地方公費を備考表示
           IF      PARA-KOHKBN         =   "1"
               PERFORM 40012-USER-KOHBIKOHEN-SEC
           END-IF
      *
           .
       40011-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    地方公費備考編集処理
      *****************************************************************
       40012-USER-KOHBIKOHEN-SEC           SECTION.
      *
      *@@@ 埼玉県対応
      *    負担者番号編集なし
           IF     SYS-1001-PREFNUM     =   "11"
               IF      COMB-KOHHKNNUM (IDX-K)
                                           =   "342"
                                           OR  "343"
                                           OR  "344"
                   MOVE    SPACE           TO  PTKOH-FTNJANUM
               END-IF
           END-IF
      *@@@ 埼玉県対応
      *
           IF      PTKOH-KOHNUM        >=  "100"
                                   AND <=  "899"
               ADD     1               TO  IDX-K3
               STRING  COMB-KOH-TANSEIDONAME (IDX-K)
                                           DELIMITED  BY  SPACE
                       "　負担者番号："    DELIMITED  BY  SIZE
                       PTKOH-FTNJANUM      DELIMITED  BY  SPACE
                       "　受給者番号："    DELIMITED  BY  SIZE
                       PTKOH-JKYSNUM       DELIMITED  BY  SPACE
                                       INTO    WRK-HEN-KOH(IDX-K3)
               END-STRING
               MOVE    SPACE               TO  WRK-JKYSNO
               MOVE    ZERO                TO  WRK-JKYSNO-MAX
               MOVE    SPACE               TO  WRK-FTNSNO
               MOVE    1                   TO  FLG-KOH-NO
           END-IF
           .
       40012-USER-KOHBIKOHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢非該当の判定処理
      *****************************************************************
       40015-PTKOHINF-976CHK-SEC      SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    "976"               TO  PTKOH-KOHNUM
           MOVE    SPA-SRYYMD          TO  PTKOH-TEKSTYMD
           MOVE    SPA-SRYYMD          TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    高齢非該当が当日有効
           IF      FLG-PTKOHINF        =   0
               MOVE    ZERO                TO  FLG-RJNHKN
           END-IF
           .
       40015-PTKOHINF-976CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ設定編集処理
      *****************************************************************
       4002-USER-SETHEN-SEC      SECTION.
      *
      *    保険医名称
           IF      PARA-DRKBN          =   "1"
      *        表示しない
               MOVE    SPACE               TO  WRK-DRNAME
      *        麻薬施用者免許番号をクリア
               MOVE    SPACE               TO  SYS-1010-MAYAKULICENSE
           ELSE
               MOVE    SYS-1010-NAME       TO  WRK-DRNAME
           END-IF
      *
      *    交付年月日
           IF      PARA-KOFYMDKBN      =   "1"
               MOVE    SPACE               TO  WRK-KOFYMD
           ELSE
               MOVE    SPA-SRYYMD          TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-KOFYMD
           END-IF
      *
      *    負担割合
           MOVE    SPACE               TO  WRK-HEN-FTNRATE
           MOVE    SPACE               TO  WRK-HEN-FTNWARI
           EVALUATE    PARA-FTNKBN
               WHEN    "1"
                   MOVE    AGECHK-FTNRATE      TO  WRK-FTN-ZZ9
                   MOVE    WRK-FTN-ZZ9         TO  WRK-HEN-FTNRATE
                   MOVE    "％"                TO  WRK-HEN-FTNWARI
               WHEN    "2"
                   COMPUTE WRK-FTNRATE     =   AGECHK-FTNRATE  /  10
                   IF      WRK-FTNRATE-S2  =   ZERO
      *                小数以下なし
                       MOVE    WRK-FTNRATE         TO  WRK-FTN-ZZ9
                       MOVE    WRK-FTN-ZZ9         TO  WRK-HEN-FTNRATE
                   ELSE
      *                小数以下あり
                       MOVE    WRK-FTNRATE         TO  WRK-FTN-Z92
                       MOVE    WRK-FTN-Z92         TO  WRK-HEN-FTNRATE
                   END-IF
                   MOVE    "割"                TO  WRK-HEN-FTNWARI
           END-EVALUATE
      *    診療科名
           IF      PARA-SRYKAKBN       =   "1"
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-HEN-SRYKANAME
           ELSE
               MOVE    SPACE               TO  WRK-HEN-SRYKANAME
           END-IF
      *
      *    地方公費を備考へ（コメントの最初へ）
           PERFORM VARYING     IDX-K3  FROM    4   BY  -1
                   UNTIL      (IDX-K3      <   1   )
               IF      (WRK-HEN-KOH (IDX-K3)   NOT =   SPACE )
                   PERFORM VARYING     IDX-B1  FROM    9   BY  -1
                           UNTIL      (IDX-B1      <   1   )
                       IF      IDX-B1          =   1
                           MOVE    WRK-HEN-KOH (IDX-K3)
                                               TO  WRK-BIKOU-NAME
                                                          (IDX-B1)
                           ADD     1                   TO  IDX-BIKOU
                       ELSE
                           MOVE    WRK-BIKOU-NAME (IDX-B1 - 1)
                                               TO  WRK-BIKOU-NAME
                                                          (IDX-B1)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *!!
      *    備考禁忌編集
           IF      PARA-TABOO          =   "1"
      *        禁忌１
               MOVE    PTINF-TABOO-1       TO  WRK-COMMENT-HEN
               PERFORM 40021-COMMENT-HENSYU-SEC
      *        禁忌２
               MOVE    PTINF-TABOO-2       TO  WRK-COMMENT-HEN
               PERFORM 40021-COMMENT-HENSYU-SEC
           END-IF
      *    備考アレルギー編集
           IF      PARA-ALLERGY        =   "1"
      *        アレルギー１
               MOVE    PTINF-ALLERGY-1     TO  WRK-COMMENT-HEN
               PERFORM 40021-COMMENT-HENSYU-SEC
      *        アレルギー２
               MOVE    PTINF-ALLERGY-2     TO  WRK-COMMENT-HEN
               PERFORM 40021-COMMENT-HENSYU-SEC
           END-IF
      *    備考感染症編集
           IF      PARA-KANSENSYO      =   "1"
      *        感染症１
               MOVE    PTINF-KANSENSYO-1   TO  WRK-COMMENT-HEN
               PERFORM 40021-COMMENT-HENSYU-SEC
      *        感染症２
               MOVE    PTINF-KANSENSYO-2   TO  WRK-COMMENT-HEN
               PERFORM 40021-COMMENT-HENSYU-SEC
           END-IF
      *    患者コメントを備考へ
           IF      PARA-COMMENT        =   "1"
               IF      PTINF-COMMENT-1         =   SPACE
                   MOVE    PTINF-COMMENT-2     TO  WRK-PTCOMMENT (1)
               ELSE
                   MOVE    PTINF-COMMENT-1     TO  WRK-PTCOMMENT (1)
                   MOVE    PTINF-COMMENT-2     TO  WRK-PTCOMMENT (2)
               END-IF
           END-IF
      *!!
           .
       4002-USER-SETHEN-EXT.
           EXIT.
      *****************************************************************
      *    備考コメント編集処理
      *****************************************************************
       40021-COMMENT-HENSYU-SEC           SECTION.
      *
           IF      WRK-COMMENT-HEN NOT =   SPACE
               PERFORM VARYING     IDX-B1  FROM    1   BY  1
                       UNTIL      (IDX-B1      >   9   )
                   IF      WRK-BIKOU-NAME (IDX-B1) =   SPACE
                       MOVE    WRK-COMMENT-HEN     TO  WRK-BIKOU-NAME
                                                          (IDX-B1)
                       MOVE    IDX-B1              TO  IDX-BIKOU
                       MOVE    9                   TO  IDX-B1
                   END-IF
               END-PERFORM
           END-IF
           .
       40021-TABOO-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC02-HEN-SEC           SECTION.
      *
           INITIALIZE                  HC02
      *
      *    患者番号
           MOVE    SPA-PTNUM           TO  HC02-PTNUM
      *    患者名称カナ
           IF    ( PTINF-KANANAME (31 : )  =   SPACE )
               MOVE    PTINF-KANANAME      TO  HC02-KANANAME
           ELSE
               MOVE    PTINF-KANANAME      TO  HC02-KANANAME2
           END-IF
      *    患者名称
           IF    ( PTINF-NAME (27 : )  =   SPACE )
               MOVE    PTINF-NAME      TO  HC02-NAME
           ELSE
               MOVE    PTINF-NAME      TO  HC02-NAME2
           END-IF
      *
           MOVE    SPA-SEX             TO  HC02-SEX
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HC02-BIRTH
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HC02-HOSPADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HC02-HOSPNAME
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HC02-HOSPTEL
      *    ドクター
           MOVE    WRK-DRNAME          TO  HC02-DRNAME
      *
      *---(04.05.01) LINE ADD START ------------------------------------
      *    県番号
           MOVE    WRK-PREFNUM-J       TO  HC02-PREFNUM
      *    点数表区分
           MOVE    WRK-TENHYOKBN-J     TO  HC02-TENHYOKBN
      *    医療機関コード
           MOVE    WRK-HOSPCD-J        TO  HC02-HOSPCD
      *
      *    保険者情報
      *    保険者番号
           MOVE    WRK-HKNJA-G         TO  HC02-HKNJA
      *    記号・番号
           MOVE    WRK-HEN-KIGONUM     TO  HC02-KIGONUM
      *---(04.02.00) LINE ADD START ------------------------------------
      *    後期高齢者は番号のみ
           IF      COMB-HKNNUM         =   "039"
               MOVE    WRK-NUM             TO  HC02-KIGONUM
           ELSE
      *        記号・番号で１６文字以上は別行に編集
               IF      WRK-HEN-KIGONUM(33:)    NOT =   SPACE
                   MOVE    SPACE               TO  HC02-KIGONUM
                   MOVE    WRK-KIGONUM         TO  HC02-KIGO
                   MOVE    WRK-NUM             TO  HC02-NUM
               END-IF
           END-IF
      *---(04.02.00) LINE ADD END   ------------------------------------
      *    本人家族区分
           EVALUATE    PTHKN-HONKZKKBN
               WHEN    "1"
                   MOVE    "被保険者"      TO  HC02-HONKZKKBN
               WHEN    "2"
                   MOVE    "被扶養者"      TO  HC02-HONKZKKBN
           END-EVALUATE
      *
      *    交付年月日
           MOVE    WRK-KOFYMD          TO  HC02-KOFYMD
      *
      *    公費負担者番号
           MOVE    WRK-HEN-FTNJANUM (1)    TO  HC02-FTNJA
      *    受給者番号
           EVALUATE    WRK-JKYSNUM-MAX (1)
           WHEN    0   THRU   14
      *        ７文字まで
               MOVE    WRK-HEN-JKYSNUM(1)  TO  HC02-JKYSNUM
           WHEN    15  THRU   20
      *       １０文字まで
               MOVE    WRK-HEN-JKYSNUM(1)  TO  HC02-JKYSNUMN
           WHEN    21   THRU   24
      *       １２文字まで
               MOVE    WRK-HEN-JKYSNUM(1)  TO  HC02-JKYSNUM12
           WHEN    OTHER
      *       １２文字以上（１５文字まで）
               MOVE    WRK-HEN-JKYSNUM(1)  TO  HC02-JKYSNUM15
           END-EVALUATE
      *    公費負担者番号２
           MOVE    WRK-HEN-FTNJANUM(2)     TO  HC02-FTNJA2
      *    受給者番号２
           EVALUATE    WRK-JKYSNUM-MAX (2)
           WHEN    0   THRU   14
      *        ７文字まで
               MOVE    WRK-HEN-JKYSNUM(2)  TO  HC02-JKYSNUM2
           WHEN    15  THRU   20
      *       １０文字まで
               MOVE    WRK-HEN-JKYSNUM(2)  TO  HC02-JKYSNUM2N
           WHEN    21   THRU   24
      *       １２文字まで
               MOVE    WRK-HEN-JKYSNUM(2)  TO  HC02-JKYSNUM212
           WHEN    OTHER
      *       １２文字以上（１５文字まで）
               MOVE    WRK-HEN-JKYSNUM(2)  TO  HC02-JKYSNUM215
           END-EVALUATE
      *
      *    処方内容
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   LINE-MAX
               MOVE    WRK-NAIYOU (IDX IDX2)   TO  HC02-NAIYO (IDX2)
           END-PERFORM
      *
      *    パラメタ設定による編集
           PERFORM 30011-USER-HENSYU-SEC
      *
      *備考欄
      *    ３歳未満・老人負担割合
           MOVE    ZERO                TO  IDX-B
           IF      WRK-BIKOU       NOT =   SPACE
               ADD     1               TO  IDX-B
               MOVE    WRK-BIKOU       TO  HC02-BIKOH (IDX-B)
           END-IF
      *
      *    麻薬編集
           IF      FLG-MAYAKU          =   1
               PERFORM 31020-MAYAKU-HEN-SEC
           END-IF
      *
      *    備考欄コメント編集
           IF      IDX-BIKOU           >   ZERO
               PERFORM 3009-COMMENT-HEN-SEC
           END-IF
      *!!
      *    備考欄患者コメント編集
           IF      WRK-PTCOMMENT-G NOT =   SPACE
               PERFORM 30011-PTCOMMENT-HEN-SEC
           END-IF
      *
      *---(04.02.00) LINE UPD START ------------------------------------
      *    後発医薬品変更可
           IF      SPA-SRYYMD          <   "20080401"
               PERFORM 30010-KOUHATU-HEN-SEC
           ELSE
      *        Ｈ２０年４月から
               PERFORM 30010-KOUHATU-HEN20-SEC
           END-IF
      *---(04.02.00) LINE UPD END   ------------------------------------
      *
      *ＱＲ対応
      *    ＱＲイメージ編集
      *****PERFORM 2003-QRIMAGE-HEN-SEC
           .
       300-HC02-HEN-EXT.
           EXIT.
      *****************************************************************
      *        麻薬編集編集処理
      *****************************************************************
       31020-MAYAKU-HEN-SEC      SECTION.
      *
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL      (IDX-H   >   3   )  OR
                              (IDX-B   >=  BIKOU-MAX   )
               IF      WRK-HENSYU (IDX-H)  NOT =   SPACE
                   ADD     1                   TO  IDX-B
                   MOVE    WRK-HENSYU (IDX-H)  TO  HC02-BIKOH (IDX-B)
               END-IF
           END-PERFORM
      *
           .
       31020-MAYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    備考欄コメント編集処理
      *****************************************************************
       3009-COMMENT-HEN-SEC      SECTION.
      *
           PERFORM VARYING     IDX-BIKOU   FROM    1   BY  1
                   UNTIL      (IDX-BIKOU   >   18  )  OR
                              (IDX-B       >=  BIKOU-MAX   )
               IF      WRK-BIKOU-NAME2(IDX-BIKOU)  NOT =   SPACE
                   ADD     1                   TO  IDX-B
      *            ７行目から２５文字編集
                   IF      IDX-B               <   BIKOU-MAX2
                       MOVE    WRK-BIKOU-NAME2(IDX-BIKOU)
                                               TO  HC02-BIKOH (IDX-B)
                   ELSE
                       MOVE    WRK-BIKOU-NAME2(IDX-BIKOU)(1:50)
                                               TO  HC02-BIKOH (IDX-B)
                       IF      WRK-BIKOU-NAME2(IDX-BIKOU)(51:)
                                               NOT =   SPACE
                           ADD     1           TO  IDX-B
                           IF      IDX-B       <=  BIKOU-MAX
                              MOVE    WRK-BIKOU-NAME2(IDX-BIKOU)(51:)
                                               TO  HC02-BIKOH (IDX-B)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3009-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     患者コメント備考編集処理
      *****************************************************************
       30011-PTCOMMENT-HEN-SEC      SECTION.
      *
           PERFORM VARYING     IDX-COM     FROM    1   BY  1
                   UNTIL      (IDX-COM     >   2   )  OR
                              (IDX-B       >=  BIKOU-MAX   )
               IF      WRK-PTCOMMENT  (IDX-COM)    NOT =   SPACE
                   ADD     1                   TO  IDX-B
                   MOVE    ZERO                TO  IDX-COM2
      *            ７行目までは８６文字編集
                   IF      IDX-B               <   BIKOU-MAX2
                       MOVE    86                  TO  COM-MAX
                   ELSE
      *            ７行目から２５文字編集
                       MOVE    50                  TO  COM-MAX
                   END-IF
                   MOVE    WRK-PTCOMMENT (IDX-COM)(1:COM-MAX)
                                               TO  WRK-PTCOMMENT-HEN
      *
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-PTCOMMENT (IDX-COM)(1:COM-MAX)
                                               TO  KANACHK-MAE-INPUT
                   MOVE    COM-MAX             TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-PTCOMMENT-HEN
                   COMPUTE IDX-COM2        =   KANACHK-MAX   +   1
                   IF      WRK-PTCOMMENT (IDX-COM)(IDX-COM2:)
                                               =   SPACE
                       MOVE    ZERO                TO  IDX-COM2
                   END-IF
                   MOVE    WRK-PTCOMMENT-HEN   TO  HC02-BIKOH (IDX-B)
      *            残文字を次行へ
                   IF      IDX-COM2            >   ZERO
                       ADD     1                   TO  IDX-B
                       IF      IDX-B               <=  BIKOU-MAX
                           MOVE    WRK-PTCOMMENT(IDX-COM)(IDX-COM2:)
                                               TO  HC02-BIKOH (IDX-B)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       30011-PTCOMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        麻薬編集編集処理
      *****************************************************************
       2003-MAYAKU-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-HENSYU-AREA
           MOVE    ZERO                TO  IDX-H
      *    患者住所
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO    WRK-ADRS
           END-STRING
           ADD     1                   TO  IDX-H
           MOVE    "患者住所："        TO  WRK-HENSYU  (IDX-H)
           MOVE    WRK-ADRS            TO  WRK-HENSYU  (IDX-H)(11:76)
           IF      WRK-ADRS(77:)   NOT =   SPACE
               ADD     1                   TO  IDX-H
               MOVE    WRK-ADRS(77:)       TO  WRK-HENSYU (IDX-H)(11:)
           END-IF
      *
      *    麻薬施用者免許
           ADD     1                   TO  IDX-H
           STRING  "麻薬施用者免許証番号："
                                       DELIMITED   BY  SIZE
                   SYS-1010-MAYAKULICENSE
                                       DELIMITED   BY  SPACE
                                       INTO    WRK-HENSYU  (IDX-H)
           END-STRING
      *
           .
       2003-MAYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        後発医薬品変更可編集処理
      *****************************************************************
       30010-KOUHATU-HEN-SEC      SECTION.
      *
      *    見出し
           MOVE    "後発医薬品への変更可"  TO  HC02-MIDASHI
      *
      *    変更可の署名があり、後発医薬品がある先発品がある時
           IF    ((FLG-KOUHATU-HEN     =   1 )  OR
                  (SYS-1030-KOUHATUKA  =   "1"
                                       OR  "2"))  AND
                  (FLG-KOUHATU-CHK     =   1 )
      *        ドクター
               MOVE    WRK-DRNAME          TO  HC02-HKNINAME
           END-IF
      *    後発医薬品の後発医薬品への変更可
           IF    ((FLG-KOUHATU-HEN     =   1 )  OR
                  (SYS-1030-KOUHATUKA  =   "2"))  AND
                  (FLG-KOUHATU-1-CHK   =   1 )
      *        ドクター
               MOVE    WRK-DRNAME          TO  HC02-HKNINAME
           END-IF
           .
       30010-KOUHATU-HEN-EXT.
           EXIT.
      *---(04.02.00) LINE ADD START ------------------------------------
      *****************************************************************
      *        後発医薬品変更可編集処理（Ｈ２０．４から）
      *****************************************************************
       30010-KOUHATU-HEN20-SEC      SECTION.
      *
      *    見出し
           STRING  "後発医薬品（ジェネリック医薬品）への変更が全"
                   "て不可の場合、以下に署名又は記名・押印"
                                       DELIMITED  BY  SIZE
                                       INTO    HC02-MIDASHI2
           END-STRING
      *
      *    変更可の署名あり、変更不可の入力がある時
      *    後発医薬品・先発品がない
           IF     (FLG-KOUHATU-HEN     =   1  )
              OR  (FLG-KOUHATUFUKA-ARI =   1  )
      ******* OR  (FLG-KOUHATU-ARI     =   ZERO )
               CONTINUE
           ELSE
      *        変更不可の署名
               IF      (FLG-KOUHATUFUKA-IN     =   1  )
                   OR  (SYS-1030-KOUHATUKA     =   "0")
                   MOVE    WRK-DRNAME          TO  HC02-HKNINAME
               END-IF
           END-IF
           .
       30010-KOUHATU-HEN20-EXT.
           EXIT.
      *---(04.02.00) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    パラメタによる編集処理
      *****************************************************************
       30011-USER-HENSYU-SEC           SECTION.
      *
      *    診療科
           MOVE    WRK-HEN-SRYKANAME   TO  HC02-SRYKAMEI
      *    負担割合
           MOVE    WRK-HEN-FTNRATE     TO  HC02-FTNRATE
           MOVE    WRK-HEN-FTNWARI     TO  HC02-FTNWARI
      *
      *    ページ編集
           MOVE    SPACE               TO  WRK-HEN-PAGE
           IF      PARA-PAGE           =   "1"
               MOVE    IDX                 TO  WRK-HEN-PAGE1
               MOVE    WRK-PAGE-MAX        TO  WRK-HEN-PAGE2
               STRING  WRK-HEN-PAGE1
                       "/"
                       WRK-HEN-PAGE2       DELIMITED  BY  SIZE
                                       INTO    WRK-HEN-PAGE
               END-STRING
               MOVE    WRK-HEN-PAGE        TO  HC02-PAGE
           END-IF
           .
       30011-USER-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1002-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    患者情報
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               INITIALIZE              PTINF-REC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *---(04.03.00) LINE ADD START ------------------------------------
      *    退院時院外処方せん（入院）
           IF      ORCHC19-KBN         =   "N"
               INITIALIZE                      SYS-1034-REC
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "2"                 TO  ORCSPRTNM-KBN
               MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
               MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
               CALL    "ORCSPRTNM"         USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYS-1034-REC
           END-IF
      *---(04.03.00) LINE END START ------------------------------------
      *
      *    処方箋ＧＥ出力情報編集
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1030-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1030-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-SYS1900-SYORI-SEC
      *
      *---(04.05.01) LINE ADD START ------------------------------------
      *    県番号
           MOVE    SYS-1001-PREFNUM    TO  WRK-MOJI
           MOVE    2                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:4)  TO  WRK-PREFNUM-J
      *    点数表区分
           MOVE    SYS-1001-TENHYOKBN  TO  WRK-MOJI
           MOVE    1                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:2)  TO  WRK-TENHYOKBN-J
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  WRK-MOJI
           MOVE    7                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:14) TO  WRK-HOSPCD-J
      *
      *    ＱＲ空白画像位置
      *    INITIALIZE                  ORCSMKPASSAREA
      *    MOVE    KUHAKU-NAME         TO  MKPASS-IN-01
      *    CALL    "ORCSMKPASS"        USING
      *                                ORCSMKPASSAREA
      **** MOVE    MKPASS-OT-01        TO  KUHAKU-FILE-NAME
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       200-MAIN-SYORI-SEC                   SECTION.
      *
      *    保険選択がない時、見出しのみとする
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "2"                 TO  ORCHC19-KBN
           END-IF
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *    院内・院外判定
           MOVE    ZERO                TO  FLG-SYOHO-IN
           EVALUATE    ORCHC19-KBN
      *        院外
               WHEN    SPACE
      *            請求確認
               WHEN    "1"
      *            前回処方（受付）
               WHEN    "4"
      *            中途終了
               WHEN    "B"
      *            前回処方
                   MOVE    1               TO  FLG-SYOHO-IN
      *        院内
               WHEN    "3"
      *            請求確認
               WHEN    "5"
      *            中途終了
               WHEN    "C"
      *            前回処方
                   MOVE    2               TO  FLG-SYOHO-IN
           END-EVALUATE
      *
           EVALUATE    ORCHC19-KBN
               WHEN    "1"
      *            前回処方印刷
                   IF      ORCHC19-DENPNUM     =   ZERO
      *                受付から
                       PERFORM 2001-RENNUM-SET-SEC
                   ELSE
                       PERFORM 2005-ZENKAI-SYORI-SEC
                   END-IF
               WHEN    "2"
      *            見出しのみ
                   INITIALIZE                  WRK-SYOHOU-AREA
                   MOVE    1                   TO  WRK-PAGE-MAX
                   MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
                   INITIALIZE                  ORCSQRCSVAREA
                   PERFORM 400-PRINT-SYORI-SEC
               WHEN    "4"
               WHEN    "5"
      *            中途終了時、発行
                   PERFORM 2003-WKSRYACT-SYORI-SEC
               WHEN    "A"
      *            前回処方（内容変更）、退院時院外処方
               WHEN    "D"
      *            前回処方（日数ゼロ）（選択）
      *---(04.03.00) LINE ADD START ------------------------------------
               WHEN    "N"
      *            退院時処方せん
      *---(04.03.00) LINE ADD END   ------------------------------------
                   PERFORM 2004-HENKOU-SYORI-SEC
               WHEN    "B"
               WHEN    "C"
      *            前回処方（日数ゼロ）（選択なし）
                   PERFORM 2005-ZENKAI-SYORI-SEC
               WHEN    OTHER
      *            診療行為登録時印刷
                   PERFORM 2002-DENPNUM-SYORI-SEC
           END-EVALUATE
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為登録時印刷処理
      *****************************************************************
       2002-DENPNUM-SYORI-SEC                   SECTION.
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ORCHC19-DENPNUM     TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           IF      JYURRK-GRP-DENPNUM  =   ZERO
      *         単独分
                PERFORM 20021-ONLY-SYORI-SEC
           ELSE
      *         複数分
                PERFORM 20022-FUKU-SYORI-SEC
           END-IF
      *
           .
       200-SYOHOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    複数分印刷処理
      *****************************************************************
       20022-FUKU-SYORI-SEC          SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    受診履歴検索
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *    処方編集初期クリア処理
           PERFORM 200211-SYOHOU-INIT-SEC
           MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
           PERFORM
               UNTIL   FLG-JYURRK      =   1
               IF      WRK-DENPNUM     NOT =   JYURRK-DENPNUM
      *            伝票番号毎に印刷
                   PERFORM 200212-PRINT-CHK-SEC
      *            初期編集
                   PERFORM 200211-SYOHOU-INIT-SEC
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
               END-IF
      *        受診履歴より編集
               PERFORM 20023-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    伝票番号毎に印刷
           PERFORM 200212-PRINT-CHK-SEC
      *
           .
       20022-FUKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方編集初期クリア処理
      *****************************************************************
       200211-SYOHOU-INIT-SEC          SECTION.
      *
           MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
           MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
           MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
           MOVE    JYURRK-DRCD         TO  WRK-DRCD
      *    共通クリア
           PERFORM 200211-KYOTU-INIT-SEC
           .
       200211-SYOHOU-INIT-EXT.
           EXIT.
      *****************************************************************
      *    処方編集初期共通クリア処理
      *****************************************************************
       200211-KYOTU-INIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    1                   TO  WRK-PAGE-MAX
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  FLG-MAYAKU
           MOVE    ZERO                TO  FLG-KOUHATU-HEN
           MOVE    ZERO                TO  FLG-KOUHATU-CHK
           MOVE    ZERO                TO  FLG-KOUHATU-1-CHK
      *---(04.02.00) LINE ADD START ------------------------------------
           MOVE    ZERO                TO  FLG-KOUHATUFUKA-IN
           MOVE    ZERO                TO  FLG-KOUHATUFUKA-ARI
           MOVE    ZERO                TO  FLG-KOUHATU-ARI
      *---(04.02.00) LINE ADD END   ------------------------------------
           INITIALIZE                  WRK-BIKOU-AREA
      *
      *ＱＲ対応
           INITIALIZE                  ORCSQRCSVAREA
           MOVE    ZERO                TO  IDX-QR1
           MOVE    ZERO                TO  IDX-QRB
      *    時間
           ADD     1                   TO  WRK-FILE-CNT2
           COMPUTE WRK-SYS-TIME        =   SYS-TIME
                                       +   WRK-FILE-CNT2
      *
           .
       200211-KYOTU-INIT-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       20021-ONLY-SYORI-SEC          SECTION.
      *
      *    処方編集初期クリア処理
           PERFORM 200211-SYOHOU-INIT-SEC
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *        受診履歴より編集
               PERFORM 20023-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    印刷判定
           PERFORM 200212-PRINT-CHK-SEC
      *
           .
       200-SYOHOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴からの判定編集処理
      *****************************************************************
       20023-JYURRK-HEN-SEC          SECTION.
      *
      *    包括保険分は対象外
           IF      JYURRK-HKNCOMBINUM  =   "9999"
               MOVE    ZERO            TO  JYURRK-ZAINUM (01)
           END-IF
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )   OR
                              (JYURRK-ZAINUM (IDX-JYU) =   ZERO )
               MOVE    JYURRK-ZAINUM (IDX-JYU) TO  WRK-ZAINUM
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
               IF      IDX-REN         >   4
                   MOVE    4               TO  IDX-REN
               END-IF
               PERFORM 2002-SRYACT-SYORI-SEC
           END-PERFORM
      *
           .
       20023-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処方印刷判定処理
      *****************************************************************
       200212-PRINT-CHK-SEC          SECTION.
      *
      *    対象明細がない時、出力しない
           IF      IDX                 =   ZERO
               EVALUATE    ORCHC19-KBN
                   WHEN    SPACE
      *            院外処方
      *                処方箋料がある時、見出しのみ印刷とする
                       IF      FLG-SYOHOU          =   1
                           MOVE    1                   TO  WRK-PAGE-MAX
                       ELSE
                           MOVE    ZERO                TO  WRK-PAGE-MAX
                       END-IF
                   WHEN    "1"
                   WHEN    "2"
      *                前回処方と頭書
                       MOVE    1                   TO  WRK-PAGE-MAX
                   WHEN    "A"
                   WHEN    "B"
                   WHEN    "C"
                   WHEN    "D"
      *                前回処方
                       MOVE    1                   TO  WRK-PAGE-MAX
                   WHEN    OTHER
                       MOVE    ZERO                TO  WRK-PAGE-MAX
               END-EVALUATE
               MOVE    ZERO            TO  FLG-PRINT
           ELSE
      *        最終行のメッセージ編集
               IF     (WRK-NAIYOU (WRK-PAGE-MAX IDX)   =   SPACE ) AND
                      (IDX             =   LINE-MAX  )
                   MOVE    "　　　　　　以下余白"
                                       TO  WRK-NAIYOU (WRK-PAGE-MAX IDX)
               END-IF
               ADD     1               TO  IDX
               IF      IDX             <=  LINE-MAX
                   MOVE    "　　　　　　以下余白"
                                       TO  WRK-NAIYOU (WRK-PAGE-MAX IDX)
               END-IF
               MOVE    1               TO  FLG-PRINT
           END-IF
      *
      *    総頁数から印刷を判定する
           IF      WRK-PAGE-MAX        >   ZERO
      *        印刷
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
      *
           .
       200212-PRINT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の連番確定処理
      *****************************************************************
       2001-RENNUM-SET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-PRINT
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
      *    処方編集初期クリア処理
           PERFORM 200211-SYOHOU-INIT-SEC
           MOVE    SPA-DRCD            TO  WRK-DRCD
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
           MOVE    ZERO                TO  FLG-SYORI
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
               IF      JYURRK-EDANUM       =   1
                   IF      IDX             >   ZERO
      *                伝票番号毎に印刷
                       PERFORM 200212-PRINT-CHK-SEC
                   END-IF
                   PERFORM 200211-SYOHOU-INIT-SEC
                   MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
                   MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
                   IF      JYURRK-SRYKA        =   SPA-SRYKA
                       MOVE    SPA-DRCD            TO  WRK-DRCD
                   END-IF
                   IF      SPA-HKNCOMBINUM     NOT =   SPACE
                       MOVE    SPA-HKNCOMBINUM TO   WRK-HKNCOMBINUM
                   END-IF
                   MOVE    ZERO                TO  FLG-SYORI
                   IF      FLG-PRINT           =   ZERO
                       IF      SPA-HKNCOMBINUM     =   SPACE
                           MOVE    JYURRK-HKNCOMBINUM  TO
                                                       WRK-HKNCOMBINUM
                           MOVE    1                   TO  FLG-SYORI
                       ELSE
                           IF      SPA-HKNCOMBINUM
                                               =   JYURRK-HKNCOMBINUM
                               MOVE    1               TO  FLG-SYORI
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        編集
               IF      FLG-SYORI           =   1
      *            受診履歴より編集
                   PERFORM 20023-JYURRK-HEN-SEC
               END-IF
               IF      FLG-PRINT           =   1
                   MOVE    1               TO  FLG-JYURRK
               ELSE
                   MOVE    ZERO            TO  FLG-JYURRK
                   MOVE    "tbl_jyurrk"    TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    受診履歴なし（見出しのみ）
           IF      FLG-PRINT           =   ZERO
               IF      WRK-PAGE-MAX        =   ZERO
                   MOVE    1                   TO  WRK-PAGE-MAX
               END-IF
      *        伝票番号毎に印刷
               PERFORM 200212-PRINT-CHK-SEC
           END-IF
      *
           .
       2001-RENNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACT-SYORI-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *    剤毎のクリア
           PERFORM 20020-INIT-ZAINUM-SEC
      *
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        在宅・投薬
               IF     (SRY-SRYKBN          =   "14"
                                           OR  "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "98" )
                   ADD     1               TO  IDX-SRY
                   MOVE    SRYACT-REC      TO  TBL-SRYACT-REC (IDX-SRY)
               END-IF
      *
      *        処方箋料の算定判定
               IF      SRY-SRYSYUKBN       =   "820"
                   MOVE    1                   TO  FLG-SYOHOU
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-SRY             >   ZERO
               PERFORM 20021-SRYACT-CHK-SEC
           END-IF
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    剤毎の編集項目クリア処理
      *****************************************************************
       20020-INIT-ZAINUM-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  WRK-ZAITENKEI
           MOVE    ZERO                TO  WRK-KIZAITEN
           MOVE    ZERO                TO  WRK-YKZTEN
      *
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU-ARI
           MOVE    ZERO                TO  FLG-HENSYU
           MOVE    ZERO                TO  FLG-SOURYOHEN
           INITIALIZE                  WRK-MEISAI-AREA
           INITIALIZE                  TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-SRY
      *    ＱＲ対応
           MOVE    ZERO                TO  IDX-QR2
           .
       20020-INIT-ZAINUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の対象有無チェック処理
      *****************************************************************
       20021-SRYACT-CHK-SEC         SECTION.
      *
           MOVE    TBL-SRYACT-REC (1)  TO  SRYACT-REC
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
      *
           MOVE    ZERO                TO  FLG-TAIGAI
           MOVE    ZERO                TO  FLG-ZAITAKU-ARI
      *    院内の在宅判定
           IF     (FLG-SYOHO-IN        =   2  )  AND
                  (WRK-SRYKBN          =   "14" )
      *        薬剤の有無
               PERFORM VARYING     IDXB    FROM    1   BY  1
                       UNTIL      (IDXB    >   IDX-SRY   )
                               OR (FLG-ZAITAKU-ARI =   1 )
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )   OR
                                      (TBL-SRY-SRYCD (IDXB IDXA)
                                                       =   SPACE)
                                   OR (FLG-ZAITAKU-ARI =   1 )
                       IF     (TBL-SRY-SRYCD (IDXB IDXA)(1:1)
                                                       =   "6"  OR
                                                           "7" )  OR
                              (TBL-SRY-SRYCD (IDXB IDXA)(1:3)
                                                       =   "059" )
                           MOVE    1               TO  FLG-ZAITAKU-ARI
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    対象外の剤判定
           EVALUATE    FLG-SYOHO-IN
      *        院外処方
               WHEN    1
      *            院内分は対象外
                   IF      (WRK-SRYKBN         =   "14"  )
                       IF     (WRK-SRYSYUKBN      =   "148"
                                                  OR  "149" )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   END-IF
                   IF     (WRK-SRYSYUKBN       =   "213"  OR
                                                   "223"  OR
                                                   "233"  OR
                                                   "211"  OR
                                                   "221"  OR
                                                   "231"  OR
                                                   "291" )
      *                院内処方種別
                       MOVE    1               TO  FLG-TAIGAI
                   END-IF
      *        院内処方
               WHEN    2
      *            院外は対象外
                   IF      (WRK-SRYKBN         =   "14"  )
                       IF     (WRK-SRYSYUKBN       =   "148"
                                                   OR  "149" )
                          OR  (FLG-ZAITAKU-ARI     =   ZERO  )
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   END-IF
                   IF     (WRK-SRYSYUKBN       =   "212"  OR
                                                   "222"  OR
                                                   "232"  OR
                                                   "292")
                       MOVE    1               TO  FLG-TAIGAI
                   END-IF
           END-EVALUATE
      *
           IF      FLG-TAIGAI          =   ZERO
      *        診療会計より、点数チェック
               PERFORM 20021-SRYACCT-SYORI-SEC
           END-IF
           IF      FLG-TAIGAI          =   ZERO
      *        点数がゼロのものを院外処方分とする
               IF     (WRK-SRYKBN          =   "21"
                                           OR  "22"
                                           OR  "23" )
               AND   ((WRK-SRYSYUKBN       =   SPACE )
                  OR  (WRK-SRYSYUKBN(3:1)  =   "0"   ))
                   IF     (FLG-SYOHO-IN        =   2    )
      *                剤点数ゼロで院内処方印刷
                       IF     (ACCT-ZAITEN         =   ZERO )
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   ELSE
                       IF     (ACCT-ZAITEN     NOT =   ZERO )
      *                    院外処方印刷で点数あり
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   END-IF
      ****         院内包括は対象
      *            IF      WRK-SRYSYUKBN       =   "213"
      *                                        OR  "223"
      *                                        OR  "233"
      *                MOVE    ZERO            TO  FLG-TAIGAI
      *****        END-IF
                END-IF
           END-IF
      *
      *    処方せん印刷処理
           INITIALIZE                  WRK-SRYACT-GRP-AREA
           IF      FLG-TAIGAI          =   ZERO
               PERFORM VARYING     IDXB    FROM    1   BY  1
                       UNTIL      (IDXB    >   IDX-SRY   )
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )
                       IF     (TBL-SRY-SRYCD (IDXB IDXA)   =   SPACE)
                          OR  (TBL-SRY-SRYCD (IDXB IDXA)(1:4)
                                                           =   "0086")
                           CONTINUE
                       ELSE
                           ADD     1               TO  WRK-SRY-MAX
                           MOVE    TBL-SRY-SRYCD (IDXB IDXA)
                                                   TO  WRK-SRY-SRYCD
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-SRYSURYO (IDXB IDXA)
                                                   TO  WRK-SRY-SRYSURYO
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-SRYKAISU (IDXB IDXA)
                                                   TO  WRK-SRY-SRYKAISU
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-MEISKYFLG (IDXB IDXA)
                                                   TO  WRK-SRY-MEISKYFLG
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-AUTOKBN (IDXB IDXA)
                                                   TO  WRK-SRY-AUTOKBN
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-INPUTNUM (IDXB IDXA)
                                                   TO  WRK-SRY-INPUTNUM
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-JIHIMONEY (IDXB IDXA)
                                                   TO  WRK-SRY-JIHIMONEY
                                                           (WRK-SRY-MAX)
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
           IF     (WRK-SRY-MAX         >   ZERO ) AND
                  (WRK-KAISU           >   ZERO )
      *        処方せん対象
               PERFORM 20022-SYOHOU-HENSYU-SEC
           END-IF
           .
       20021-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20021-SRYACCT-SYORI-SEC         SECTION.
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
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
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      ACCT-ZAIKBN         =   1
      *        薬評分は対象外
               MOVE    1                   TO  FLG-TAIGAI
           ELSE
               MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-ACT
               MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                           TO  WRK-KAISU
           END-IF
           .
       20021-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の処方せん内容編集処理
      *****************************************************************
       20022-SYOHOU-HENSYU-SEC         SECTION.
      *
           IF      WRK-SRYKBN          =   "98"
      *        処方備考コメント
               PERFORM 20030-SRYKBN-98-HEN-SEC
           ELSE
      *        処方せん内容
               PERFORM 20040-MEISAI-HEN-SEC
           END-IF
           .
       20022-SYOHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の処方せん内容編集処理
      *****************************************************************
       20040-MEISAI-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   WRK-SRY-MAX      )   OR
                              (WRK-SRY-SRYCD (IDXA)    =   SPACE)
      *        対象外（器材商品コード、逓減コード、用量コード）
               IF     (WRK-SRY-SRYCD (IDXA)(1:3) =   "058" )
                  OR  (WRK-SRY-SRYCD (IDXA)      =   "630010002" )
                  OR  (WRK-SRY-SRYCD (IDXA)(1:7) =   "0992000" )
                  OR  (WRK-SRY-SRYCD (IDXA)(1:7) =   "0999999" )
                   CONTINUE
               ELSE
                   IF     (WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"
                                                       OR  "7"
                                                       OR  "0"
                                                       OR  "8" )
                       PERFORM 3103-SRYACT-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    剤ごとに、編集する
           IF      IDX-S               =   ZERO
               CONTINUE
           ELSE
               PERFORM 20029-SRYACT-HENSYU-SEC
           END-IF
      *
           .
       20040-MEISAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       3103-SRYACT-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-KOUHATU-1
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-FUKA
      *
           IF      WRK-SRY-SRYCD (IDXA)    =   "099209903"
                                           OR  "099209905"
                                           OR  "099209906"
      *        後発医薬品へ変更不可（ＱＲ）
      *        （含量規格変更不可・剤型変更不可）
               PERFORM 31039-QR-KOUHATU-SEC
           ELSE
               IF      WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"
      *            薬剤の下変更不可コード判定
                   PERFORM 310391-FUKACHK-SEC
               END-IF
      *        コード毎の編集
               PERFORM 31032-TENSU-HENSYU-SEC
           END-IF
           .
       3103-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の編集処理
      *****************************************************************
       20029-SRYACT-HENSYU-SEC         SECTION.
      *
      *---(03.04.00) LINE ADD START ------------------------------------
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF     (WRK-SRYKBN          =   "21"  )  AND
                  (FLG-SOURYOHEN       =   1     )
               MOVE    "23"                TO  WRK-SRYKBN
           END-IF
      *---(03.04.00) LINE ADD END   ------------------------------------
           EVALUATE    WRK-SRYKBN
           WHEN    "21"
           WHEN    "22"
      *    内服・頓服
      *        最後の行に回数を編集
               IF      WRK-SRYCD (1:1)     =   "6"
                   ADD     1                   TO  IDX-S
               ELSE
      *            名称が回数表示まで編集されている時、行追加
                   IF      WRK-M-KETA      >   66    AND
                                           <=  80
                       ADD     1                   TO  IDX-S
                   END-IF
               END-IF
           END-EVALUATE
      *    回数行で２０行以上の時
           IF      IDX-S               >=  LINE-MAX
               COMPUTE IDX-S           =   LINE-MAX   -   1
      *        帳票編集処理
               PERFORM 20022-PRINT-HEN-SEC
               MOVE    SPACE               TO  WRK-NAIYOU-AREA
               MOVE    1                   TO  IDX-S
           END-IF
      *
      *    最後の行に回数を編集
           IF      ORCHC19-KBN         =   "1"
                                       OR  "B"
                                       OR  "C"
                                       OR  "D"
      *        回数編集なし
               CONTINUE
           ELSE
      *        診療会計より回数編集
               EVALUATE    WRK-SRYKBN
                   WHEN    "21"
                   WHEN    "22"
                       PERFORM 20022-KAISU-HEN-SEC
               END-EVALUATE
           END-IF
      *
           EVALUATE    WRK-SRYKBN
               WHEN    "21"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "日分）"              TO  WRK-HINICHI (IDX-S)
               WHEN    "22"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "回分）"              TO  WRK-HINICHI (IDX-S)
      *******  WHEN    "23"
      *        WHEN    "14"
      *            IF      WRK-NAIYOU-Z (IDX-S)  =   SPACE
      *                COMPUTE IDX-S             =   IDX-S    -   1
      **********   END-IF
           END-EVALUATE
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-KAISUU   (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-FIL21    (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-K3       (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-MEISYOU2 (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    帳票用編集処理
           PERFORM 20022-PRINT-HEN-SEC
      *
           .
       20029-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20022-KAISU-HEN-SEC         SECTION.
      *
           MOVE    WRK-KAISU           TO  WRK-ZZZ
           MOVE    3                   TO  WRK-MOJISU
           MOVE    WRK-ZZZ             TO  WRK-MOJI
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:6)  TO  WRK-KAISUU (IDX-S)
           INSPECT WRK-KAISUU (IDX-S)  REPLACING  ALL "  "  BY  "　"
      *
           .
       20022-KAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票用編集処理
      *****************************************************************
       20022-PRINT-HEN-SEC         SECTION.
      *
      *    最大行数のセット
           COMPUTE IDX-MAX             =   LINE-MAX   -   1
      *
           IF     (IDX-S   +   IDX)    >   IDX-MAX
               MOVE    "次ページへ続く"    TO  WRK-NAIYOU
                                                (WRK-PAGE-MAX LINE-MAX)
                                                                (50:)
               IF      IDX             >   ZERO
      *            改ページ
                   ADD     1                   TO  WRK-PAGE-MAX
                   MOVE    ZERO                TO  IDX
               END-IF
           END-IF
      *
           PERFORM VARYING IDXA2       FROM    1   BY  1
                   UNTIL   IDXA2       >   IDX-S
               ADD     1                   TO  IDX
               MOVE    WRK-NAIYOU-Z (IDXA2) TO  WRK-NAIYOU
                                                    (WRK-PAGE-MAX IDX)
           END-PERFORM
           ADD     1                   TO  IDX
      *
           .
       20022-PRINT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    変更不可コード編集処理
      *****************************************************************
       310391-FUKACHK-SEC             SECTION.
      *
           INITIALIZE                      WRK-FUKA-MEI-AREA
      *
           COMPUTE IDXF2               =   IDXA    +   1
           PERFORM VARYING     IDXF    FROM    IDXF2   BY  1
                   UNTIL      (IDXF    >   WRK-SRY-MAX   )   OR
                              (WRK-SRY-SRYCD (IDXF)    =   SPACE)
               EVALUATE    WRK-SRY-SRYCD (IDXF)
                   WHEN    "099209903"
      *                後発医薬品へ変更不可
                       MOVE    1                       TO  IDXF3
                       MOVE    WRK-SRY-SRYCD (IDXF)    TO  WRK-SRYCD
                       PERFORM 31039-KOUHATU-NAME-SEC
                   WHEN    "099209905"
      *                含量規格変更不可
                       MOVE    2                       TO  IDXF3
                       MOVE    WRK-SRY-SRYCD (IDXF)    TO  WRK-SRYCD
                       PERFORM 31039-KOUHATU-NAME-SEC
                   WHEN    "099209906"
      *                剤型変更不可
                       MOVE    3                       TO  IDXF3
                       MOVE    WRK-SRY-SRYCD (IDXF)    TO  WRK-SRYCD
                       PERFORM 31039-KOUHATU-NAME-SEC
                   WHEN    OTHER
                       MOVE    WRK-SRY-MAX         TO  IDXF
               END-EVALUATE
           END-PERFORM
           .
       310391-FUKACHK-EXT.
           EXIT.
      *****************************************************************
      *    後発医薬品へ変更不可編集処理
      *****************************************************************
       31039-KOUHATU-NAME-SEC         SECTION.
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               STRING  "【"            DELIMITED  BY  SIZE
                       TNS-NAME        DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
                                       INTO    WRK-KOU-NAME (IDXF3)
               END-STRING
               COMPUTE WRK-KOU-KETA(IDXF3) =  (TNS-YUKOKETA  * 2)
                                           +   4
               MOVE    TNS-NAME            TO  WRK-KOU-TNSNAME(IDXF3)
           END-IF
           MOVE    1                   TO  FLG-KOUHATU-FUKA
      *---(04.02.00) LINE ADD START ------------------------------------
           MOVE    1                   TO  FLG-KOUHATUFUKA-ARI
      *---(04.02.00) LINE ADD END   ------------------------------------
           .
       31039-KOUHATU-NAME-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲデータ用編集処理
      *****************************************************************
       31039-QR-KOUHATU-SEC         SECTION.
      *
      *    ＱＲデータ用
           IF      IDX-QR1             >   ZERO
               PERFORM VARYING     IDXF    FROM    1   BY  1
                       UNTIL      (IDXF    >       3  )
                   IF      WRK-KOU-TNSNAME (IDXF)  NOT =   SPACE
                       ADD     1                   TO  IDX-QR2
                       MOVE    WRK-SRY-SRYCD (IDXA)
                                               TO  ORCSQRCSV-SRYCD
                                                  (IDX-QR1 IDX-QR2)
                       MOVE    WRK-KOU-TNSNAME (IDXF)
                                               TO  ORCSQRCSV-TNS-NAME
                                                  (IDX-QR1 IDX-QR2)
      *                編集後、クリア
                       MOVE    SPACE           TO  WRK-KOU-TNSNAME(IDXF)
                   END-IF
               END-PERFORM
           END-IF
           .
       31039-QR-KOUHATU-EXT.
           EXIT.
      *****************************************************************
      *    コード毎の編集処理
      *****************************************************************
       31032-TENSU-HENSYU-SEC         SECTION.
      *
      *    点数検索
           MOVE    WRK-SRY-SRYCD (IDXA)    TO  WRK-SRYCD
           MOVE    WRK-SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    内容
           IF      WRK-SRYCD  (1:1)   =   "6"
      *H24.3
      *        一般名記載なし区分
               IF      PARA-GE201203       =   "1"
                   IF      TNSP-IPN-KISAIKBN   =   1
                       MOVE    ZERO            TO  TNSP-IPN-KISAIKBN
                   END-IF
               END-IF
      *
      *        一般名・処方名称記載
               IF      TNSP-IPN-KISAIKBN   =   ZERO
                   MOVE    1               TO  WRK-KANZANCHI
               ELSE
                   PERFORM 310321-GENERIC-HEN-SEC
               END-IF
      *        ＧＥ付き表示の時、表示
               MOVE    TNS-NAME        TO  WRK-TNS-NAME
               IF      SYS-1030-SYOHOGEPRTFLG  =   1
                   IF     (TNS-SYUSAISKB       =   8   )  AND
                          (TNS-SYOMEIKANREN    NOT =   SPACE AND
                                                       ZERO )
                       COMPUTE KETA            =  (TNS-YUKOKETA  * 2)
                                               +   1
                       MOVE    "（ＧＥ）"      TO  WRK-TNS-NAME(KETA:)
                   END-IF
               END-IF
      *        後発医薬品の時、表示（優先）
               IF      SYS-1030-SYOHODSPNAME   NOT =   SPACE
      *            後発医薬品区分
                   IF      TNS-KOUHATUKBN          =   1
                       MOVE    TNS-NAME        TO  WRK-TNS-NAME
                       COMPUTE KETA            =  (TNS-YUKOKETA  * 2)
                                               +   1
                       MOVE    SYS-1030-SYOHODSPNAME
                                               TO  WRK-TNS-NAME(KETA:)
                   END-IF
               END-IF
      *
      *        点数マスタ更新前（２００６．７の更新）
               IF      SPA-DBUPDKBN        =   ZERO
      *            後発医薬品でない
                   IF      TNS-KOUHATUKBN      =   ZERO
                       MOVE    1               TO  FLG-KOUHATU
                   END-IF
               ELSE
      *        点数マスタ更新後
      *            後発医薬品がある先発品
                   IF      TNS-KOUHATUKBN      =   2
                      MOVE    1                TO  FLG-KOUHATU
                   END-IF
               END-IF
      *        後発医薬品
               IF      TNS-KOUHATUKBN      =   1
                  MOVE    1                TO  FLG-KOUHATU-1
               END-IF
      *        麻薬判定
               IF      TNS-MADOKUKBN       =   1
                   MOVE   1                TO  FLG-MAYAKU
               END-IF
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   IF      FLG-SOURYOHEN       =   ZERO
                       MOVE    1                   TO  FLG-SOURYOHEN
                   END-IF
               ELSE
                   MOVE    2                   TO  FLG-SOURYOHEN
               END-IF
           ELSE
      *        用法
               IF      WRK-SRYCD  (1:3)    =   "001"
                   PERFORM 310300-YOUHO-NAME-SEC
               ELSE
      *            その他
                   IF      WRK-SRY-COM-NAME (IDXA) =   SPACE
                       IF      WRK-SRY-INPUTNUM (IDXA) =   ZERO
                           MOVE    TNS-NAME        TO  WRK-TNS-NAME
                       ELSE
                           PERFORM 31032-PTCOM-SET-SEC
                       END-IF
                   ELSE
                       MOVE    WRK-SRY-COM-NAME (IDXA)
                                                   TO  WRK-TNS-NAME
                   END-IF
               END-IF
           END-IF
      *    内容編集
           PERFORM 31031-NAIYOU-HENSYU-SEC
      *
      *    ＱＲデータ用
           IF      WRK-SRYCD           =   "820000047"
      *        （減）は対象外
               CONTINUE
           ELSE
               PERFORM 310329-QR-MEISEI-HEN-SEC
           END-IF
           .
       31032-TENSU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    用法名称編集処理
      *****************************************************************
       310300-YOUHO-NAME-SEC         SECTION.
      *
           IF     (WRK-SRY-INPUTNUM (IDXA) =   ZERO )
             OR   (WRK-SRY-COM-NAME (IDXA) NOT =   SPACE )
               IF      WRK-SRY-COM-NAME (IDXA) NOT =   SPACE
                   MOVE    WRK-SRY-COM-NAME (IDXA) TO  WRK-YOH-NAME
               ELSE
                   MOVE    TNS-NAME                TO  WRK-YOH-NAME
               END-IF
               MOVE    SPACE               TO  WRK-TNS-NAME
               STRING     "【"             DELIMITED   BY  SIZE
                          WRK-YOH-NAME     DELIMITED   BY  "    "
                          "】"             DELIMITED   BY  SIZE
                      INTO                 WRK-TNS-NAME
               END-STRING
           ELSE
      *        患者コメントより
               MOVE    SPACE               TO  WRK-TNS-NAME
               PERFORM 31032-PTCOM-SET-SEC
               IF      WRK-TNS-NAME        =   SPACE
                   MOVE    TNS-NAME            TO  WRK-TNS-NAME
               END-IF
               MOVE    WRK-TNS-NAME        TO  WRK-YOH-NAME
               MOVE    SPACE               TO  WRK-TNS-NAME
               STRING     "【"             DELIMITED   BY  SIZE
                          WRK-YOH-NAME     DELIMITED   BY  "    "
                          "】"             DELIMITED   BY  SIZE
                      INTO                 WRK-TNS-NAME
               END-STRING
           END-IF
           .
       310300-YOUHO-NAME-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       31031-NAIYOU-HENSYU-SEC         SECTION.
      *
           ADD     1               TO  IDX-S
      *    １剤で２０行以上の時
           IF      IDX-S               >=  LINE-MAX
               COMPUTE IDX-S           =   LINE-MAX   -   1
      *        帳票編集処理
               PERFORM 20022-PRINT-HEN-SEC
               MOVE    SPACE               TO  WRK-NAIYOU-AREA
               MOVE    1                   TO  IDX-S
               MOVE    1                   TO  FLG-KAISU
           END-IF
      *
           IF     (IDX-S           =   1   )  AND
                  (FLG-KAISU       =   ZERO)
      *        １行目
               ADD     1               TO  IDX-Z
               MOVE    IDX-Z           TO  WRK-ZZ
               MOVE    2               TO  WRK-MOJISU
               MOVE    WRK-ZZ          TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:4)
                                       TO  WRK-NO     (IDX-S)
               MOVE    "）"            TO  WRK-K2     (IDX-S)
           ELSE
               MOVE    ALL "　"        TO  WRK-NO     (IDX-S)
               MOVE    ALL "　"        TO  WRK-K2     (IDX-S)
           END-IF
      *    名称編集
           PERFORM 31031-MEISYOU-HEN-SEC
      *
           IF     (WRK-SRYCD (1:1)     =   "6"  OR  "7" )  OR
                  (WRK-SRYCD (1:3)     =   "059"        )
      *        数量
               MOVE    WRK-SRY-SRYSURYO (IDXA) TO  WRK-SURYO
               IF      WRK-SURYO           =   ZERO
                   MOVE    SPACE               TO  WRK-SURYO-N
                   MOVE    SPACE               TO  WRK-SURYO-N22
               ELSE
                   PERFORM 10011-SURYO-HEN-SEC
               END-IF
      *        数量の表示位置に編集がある時、次行へ
               IF      WRK-SURYO-N22       =   SPACE
      *            １８文字まで
                   IF      WRK-SURYOU (IDX-S)  NOT =   SPACE
                       ADD     1               TO  IDX-S
                       MOVE    ALL "　"        TO  WRK-NO     (IDX-S)
                       MOVE    ALL "　"        TO  WRK-K2     (IDX-S)
                   END-IF
                   MOVE    WRK-SURYO-N         TO  WRK-SURYOU (IDX-S)
                   MOVE    TNS-TANINAME        TO  WRK-TANI (IDX-S)
               ELSE
      *            ２２文字まで
                   IF      WRK-SURYOU4(IDX-S)  NOT =   SPACE
                       ADD     1               TO  IDX-S
                       MOVE    ALL "　"        TO  WRK-NO     (IDX-S)
                       MOVE    ALL "　"        TO  WRK-K2     (IDX-S)
                   END-IF
                   MOVE    WRK-SURYO-N22       TO  WRK-SURYOU4 (IDX-S)
                   MOVE    TNS-TANINAME        TO  WRK-TANI4 (IDX-S)
               END-IF
           END-IF
      *
           INSPECT WRK-MEISYOU1 (IDX-S)  REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-SURYOU (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-TANI   (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-FIL11  (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    後発医薬品変更可能判定
           IF      FLG-KOUHATU         =   1
      *        後発医薬品のある先発品で変更不可なし
               IF      FLG-KOUHATU-FUKA    =   ZERO
                   MOVE    1               TO  FLG-KOUHATU-CHK
               END-IF
           END-IF
           IF      FLG-KOUHATU-1       =   1
      *        後発医薬品で変更不可なし
               IF      FLG-KOUHATU-FUKA    =   ZERO
                   MOVE    1               TO  FLG-KOUHATU-1-CHK
               END-IF
           END-IF
      *---(04.02.00) LINE ADD START ------------------------------------
      *    後発医薬品・先発品で変更不可のない薬品
           IF     (FLG-KOUHATU         =   1  )  OR
                  (FLG-KOUHATU-1       =   1  )
               IF      FLG-KOUHATU-FUKA    =   ZERO
                   MOVE    1               TO  FLG-KOUHATU-ARI
               END-IF
           END-IF
      *---(04.02.00) LINE ADD END   ------------------------------------
           MOVE    ZERO                TO  FLG-KOUHATU-1
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-FUKA
      *
           .
       31031-NAIYOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *     ＱＲデータ用記載処理
      *****************************************************************
       310329-QR-MEISEI-HEN-SEC           SECTION.
      *
           IF      IDX-QR2             =   ZERO
               ADD     1                   TO  IDX-QR1
               MOVE    WRK-KAISU           TO  ORCSQRCSV-KAISU (IDX-QR1)
      *        剤型区分
               EVALUATE    WRK-SRYKBN
                   WHEN    "21"
                       MOVE    "1"         TO  ORCSQRCSV-ZAIKATA
                                                              (IDX-QR1)
                   WHEN    "22"
                       MOVE    "2"         TO  ORCSQRCSV-ZAIKATA
                                                              (IDX-QR1)
                   WHEN    "23"
                       MOVE    "3"         TO  ORCSQRCSV-ZAIKATA
                                                             (IDX-QR1)
                   WHEN    "14"
                       MOVE    "5"         TO  ORCSQRCSV-ZAIKATA 
                                                              (IDX-QR1)
      *                在宅材料を医療器材とする
                       IF      WRK-SRYSYUKBN   =   "149"
                           MOVE    "6"         TO  ORCSQRCSV-ZAIKATA 
                                                              (IDX-QR1)
                       END-IF
               END-EVALUATE
           END-IF
           ADD     1                   TO  IDX-QR2
           MOVE    WRK-SRY-SRYCD (IDXA)    TO  ORCSQRCSV-SRYCD
                                                  (IDX-QR1 IDX-QR2)
           IF      WRK-SRY-SRYCD (IDXA)(1:3)   =   "001"
               MOVE    WRK-YOH-NAME        TO  ORCSQRCSV-TNS-NAME
                                                  (IDX-QR1 IDX-QR2)
      *        用法コメント区分
               MOVE    TNS-KENTAICOMMENT   TO  ORCSQRCSV-YCOMMNTKBN
                                                  (IDX-QR1 IDX-QR2)
           ELSE
               IF     (WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"   )
                 AND  (TNSP-IPN-KISAIKBN           =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    WRK-TNS-NAME    TO  ORCSQRCSV-TNS-NAME
                                                  (IDX-QR1 IDX-QR2)
               END-IF
           END-IF
           MOVE    WRK-SURYO2          TO  ORCSQRCSV-SURYOU
                                                  (IDX-QR1 IDX-QR2)
           MOVE    TNS-TANINAME        TO  ORCSQRCSV-TANINAME
                                                  (IDX-QR1 IDX-QR2)
           MOVE    TNS-YKZKBN          TO  ORCSQRCSV-YKZKBN
                                                  (IDX-QR1 IDX-QR2)
      *    一般名記載
           MOVE    TNSP-IPN-KISAIKBN   TO  ORCSQRCSV-KISAIKBN
                                                  (IDX-QR1 IDX-QR2)
      *    単位換算値
           IF      TNSP-IPN-KISAIKBN   =   ZERO
               CONTINUE
           ELSE
               MOVE    WRK-KANZANCHI       TO  ORCSQRCSV-KANZANCHI
                                                  (IDX-QR1 IDX-QR2)
           END-IF
      *    内服滴剤
           IF     (WRK-SRYKBN          =   "21"  )  AND
                  (TNSP-SOURYOHENKBN   =   1     )
               MOVE    "4"             TO  ORCSQRCSV-ZAIKATA(IDX-QR1)
           END-IF
           .
       310329-QR-MEISEI-HEN-EXT.
           EXIT.
      *
      *---(03.04.00) LINE ADD START ------------------------------------
      *****************************************************************
      *     一般名・処方名称記載処理
      *****************************************************************
       310321-GENERIC-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-KANZANCHI
           EVALUATE    TNSP-IPN-KISAIKBN
      *        一般名記載
               WHEN    1
                   MOVE    GENERIC-GENERICNAME TO  TNS-NAME
                   MOVE    GENERIC-YUKOKETA    TO  TNS-YUKOKETA
      *            単位と換算値は附加情報を優先とする
                   IF      TNSP-IPN-KANZANTANINAME =   SPACE
                       IF      GENERIC-TANINAME    NOT =   SPACE
                           MOVE    GENERIC-TANICD      TO  TNS-TANICD
                           MOVE    GENERIC-TANINAME    TO  TNS-TANINAME
                           MOVE    GENERIC-KANZANCHI   TO  WRK-KANZANCHI
                       END-IF
                   ELSE
                       MOVE    TNSP-IPN-KANZANTANICD
                                                   TO  TNS-TANICD
                       MOVE    TNSP-IPN-KANZANTANINAME
                                                   TO  TNS-TANINAME
                       MOVE    TNSP-IPN-KANZANCHI  TO  WRK-KANZANCHI
                   END-IF
      *        処方名称記載
               WHEN    2
                   MOVE    TNSP-SHONAME        TO  TNS-NAME
                   MOVE    TNSP-SHOYUKOKETA    TO  TNS-YUKOKETA
                   IF      TNSP-IPN-KANZANTANINAME NOT =   SPACE
                       MOVE    TNSP-IPN-KANZANTANICD
                                                   TO  TNS-TANICD
                       MOVE    TNSP-IPN-KANZANTANINAME
                                                   TO  TNS-TANINAME
                       MOVE    TNSP-IPN-KANZANCHI  TO  WRK-KANZANCHI
                   END-IF
           END-EVALUATE
           .
       310321-GENERIC-HEN-EXT.
           EXIT.
      *---(03.04.00) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *     名称編集処理
      *****************************************************************
       31031-MEISYOU-HEN-SEC           SECTION.
      *
           MOVE    WRK-TNS-NAME        TO  WRK-M-NAME
      *    桁数
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-M-NAME          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-M-KETA
      *    薬剤・器材編集
           IF     (WRK-SRYCD(1:1)      =   "6"  OR  "7" )  OR
                  (WRK-SRYCD(1:3)      =   "059"  )
               PERFORM 310311-MEISYOU-HEN2-SEC
           ELSE
               MOVE    WRK-TNS-NAME    TO  WRK-MEISYOU3(IDX-S)
               IF      WRK-M-KETA      >   80
                   ADD     1               TO  IDX-S
                   MOVE    ALL "　"        TO  WRK-NO     (IDX-S)
                   MOVE    ALL "　"        TO  WRK-K2     (IDX-S)
                   MOVE    WRK-TNS-NAME(81:)   TO  WRK-MEISYOU3(IDX-S)
               END-IF
           END-IF
      *
           .
       31031-MEISYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     名称編集処理
      *****************************************************************
       310311-MEISYOU-HEN2-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-HEN-NAME
           MOVE    ZERO                TO  WRK-HEN-KETA
           PERFORM VARYING     IDXF    FROM    1   BY  1
                   UNTIL      (IDXF    >       3  )
               IF      WRK-KOU-NAME (IDXF) NOT =   SPACE
                   ADD     WRK-KOU-KETA (IDXF) TO  WRK-HEN-KETA
                   MOVE    WRK-HEN-NAME        TO  WRK-HEN-NAME2
                   MOVE    SPACE               TO  WRK-HEN-NAME
                   STRING  WRK-HEN-NAME2       DELIMITED  BY  SPACE
                           WRK-KOU-NAME(IDXF)  DELIMITED  BY  SPACE
                                               INTO  WRK-HEN-NAME
                   END-STRING
               END-IF
          END-PERFORM
      *
           MOVE    SPACE               TO  WRK-TNS-NAME
           IF      (WRK-M-KETA +   WRK-HEN-KETA)  <=  80
               STRING   WRK-M-NAME     DELIMITED  BY  SPACE
                        WRK-HEN-NAME   DELIMITED  BY  SPACE
                                       INTO  WRK-TNS-NAME
               END-STRING
               MOVE    WRK-TNS-NAME    TO  WRK-MEISYOU3 (IDX-S)
           ELSE
               MOVE    WRK-M-NAME      TO  WRK-MEISYOU3 (IDX-S)
               IF      WRK-M-KETA      <=  80
                   ADD     1               TO  IDX-S
                   MOVE    ALL "　"        TO  WRK-NO (IDX-S)
                   MOVE    ALL "　"        TO  WRK-K2 (IDX-S)
                   MOVE    WRK-HEN-NAME    TO  WRK-MEISYOU3 (IDX-S)
               ELSE
                   MOVE    WRK-M-NAME(81:) TO  WRK-NAME
                   STRING  WRK-NAME        DELIMITED  BY  SPACE
                           WRK-HEN-NAME    DELIMITED  BY  SPACE
                                           INTO  WRK-TNS-NAME
                   END-STRING
                   ADD     1               TO  IDX-S
                   MOVE    ALL "　"        TO  WRK-NO (IDX-S)
                   MOVE    ALL "　"        TO  WRK-K2 (IDX-S)
                   MOVE    WRK-TNS-NAME    TO  WRK-MEISYOU3 (IDX-S)
               END-IF
           END-IF
      *
           .
       310311-MEISYOU-HEN2-EXT.
           EXIT.
      *---(01.00.14) LINE ADD START -----------------------------------
      *****************************************************************
      *    コメント（備考）（院内・院外共通）編集処理
      *****************************************************************
       20030-SRYKBN-98-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   WRK-SRY-MAX   )  OR
                              (WRK-SRY-SRYCD (IDXA)   =   SPACE)
      *---(04.02.00) LINE UPD START ------------------------------------
               IF      WRK-SRY-SRYCD (IDXA)    =   "099209902"
                                               OR  "099209904"
                   IF      WRK-SRY-SRYCD (IDXA)    =   "099209902"
      *                後発医薬品への変更
                       MOVE    1               TO  FLG-KOUHATU-HEN
                   ELSE
      *                後発医薬品への変更不可
                       MOVE    1               TO  FLG-KOUHATUFUKA-IN
                   END-IF
      *---(04.02.00) LINE UPD END   ------------------------------------
               ELSE
                   MOVE    WRK-SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            用法
                   IF      WRK-SRY-SRYCD (IDXA)(1:3)   =   "001"
                       PERFORM 310300-YOUHO-NAME-SEC
                   ELSE
      *            その他
                       IF      WRK-SRY-COM-NAME (IDXA) =   SPACE
                           IF      WRK-SRY-INPUTNUM (IDXA) =   ZERO
                               MOVE    TNS-NAME        TO  WRK-TNS-NAME
                           ELSE
                               PERFORM 31032-PTCOM-SET-SEC
                           END-IF
                       ELSE
                           MOVE    WRK-SRY-COM-NAME (IDXA)
                                                   TO  WRK-TNS-NAME
                       END-IF
                   END-IF
                   ADD     1                   TO  IDX-BIKOU
                   IF      IDX-BIKOU           <=  9
                       MOVE    WRK-TNS-NAME        TO  WRK-BIKOU-NAME
                                                           (IDX-BIKOU)
                   END-IF
      *            ＱＲデータ
                   PERFORM 20030-QR-SRYKBN98-HEN-SEC
               END-IF
           END-PERFORM
           .
       20030-SRYKBN-98-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     ＱＲ備考コメント編集処理
      *****************************************************************
       20030-QR-SRYKBN98-HEN-SEC            SECTION.
      *
           ADD     1                   TO  IDX-QRB
           IF      IDX-QRB             <=  10
               IF      WRK-SRY-SRYCD (IDXA)(1:3) =   "001"
                   MOVE    WRK-YOH-NAME    TO  ORCSQRCSV-BIKOU
                                                           (IDX-QRB)
               ELSE
                   MOVE    WRK-TNS-NAME    TO  ORCSQRCSV-BIKOU
                                                           (IDX-QRB)
               END-IF
           END-IF
           .
       20030-QR-SRYKBN98-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       10011-SURYO-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-N
           MOVE    SPACE               TO  WRK-SURYO-N22
      *    換算値により数量変更
           IF      WRK-KANZANCHI       =   ZERO
               MOVE    1               TO  WRK-KANZANCHI
           END-IF
           COMPUTE WRK-SURYO2          =   WRK-KANZANCHI
                                       *   WRK-SURYO
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO2-S2(4:2)  =   ZERO
      *        小数点３桁まで
               PERFORM 100111-SURYO-HEN03-SEC
           ELSE
      *        小数点５桁
               PERFORM 100112-SURYO-HEN05-SEC
           END-IF
      *
           .
       10011-SURYO-HEN-EXT.
           EXIT.
      *---(03.04.00) LINE ADD STRT  ------------------------------------
      *****************************************************************
      *    小数点３桁まで数量表示用編集処理
      *****************************************************************
       100111-SURYO-HEN03-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    3  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO2-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO2-S2(IDXM:1) TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           MOVE    WRK-SURYO-X         TO  WRK-MOJI
           MOVE    9                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:18) TO  WRK-SURYO-N
           .
       100111-SURYO-HEN03-EXT.
           EXIT.
      *****************************************************************
      *    小数点５桁まで数量表示用編集処理
      *****************************************************************
       100112-SURYO-HEN05-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X2
           MOVE    WRK-SURYO2-S1       TO  WRK-SURYO2-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO2-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO2-S2(IDXM:1) TO  WRK-SURYO2-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO2-Z3       NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO2-Z2
           END-IF
      *
           IF      WRK-SURYO2-X21      =   SPACE
      *        整数３桁・小数部５桁まで
               MOVE    WRK-SURYO2-X29      TO  WRK-MOJI
               MOVE    9                   TO  WRK-MOJISU
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:18) TO  WRK-SURYO-N
           ELSE
      *        整数３桁以上・小数部５桁
               MOVE    WRK-SURYO-X2        TO  WRK-MOJI
               MOVE    11                  TO  WRK-MOJISU
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:22) TO  WRK-SURYO-N22
           END-IF
           .
       100112-SURYO-HEN05-EXT.
           EXIT.
      *---(03.04.00) LINE ADD END   ------------------------------------
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       31032-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    WRK-SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    WRK-SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-TNS-NAME
           END-IF
           .
       31032-PTCOM-SET-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ZENKAKU-G
           ELSE
               MOVE    WRK-MOJI            TO  WRK-ZENKAKU-G
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *---(01.00.07) LINE ADD START ------------------------------------
      *****************************************************************
      *    中途終了時の発行処理
      *****************************************************************
       2003-WKSRYACT-SYORI-SEC         SECTION.
      *
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
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
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
           MOVE    SPA-DRCD            TO  WRK-DRCD
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
      *        保険の再編集
               IF      WKSRY-INPUTCD(1)(1:1)   =   "#"
                   MOVE    WKSRY-INPUTCD(1)(2:4)   TO  WRK-HKNCOMBINUM
               END-IF
      *        診療科の再編集
               IF      WKSRY-INPUTCD(1)(1:1)   =   "$"
                   MOVE    WKSRY-INPUTCD(1)(2:2)   TO  WRK-SRYKA
                   IF      WKSRY-INPUTCD(1)(5:4)   NUMERIC
                       MOVE    "1"                 TO  WRK-DRCD(1:1)
                       MOVE    WKSRY-INPUTCD(1)(5:4)
                                                   TO  WRK-DRCD(2:4)
                   END-IF
               END-IF
      *        投薬と在宅のみ対象とする
               IF     (WKSRY-SRYKBN        =   "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "14"
                                           OR  "98"  )
      *           包括保険分は対象外
                  AND (WRK-HKNCOMBINUM NOT =   "9999" )
      *           薬評分は対象外
                  AND  (WKSRY-ZAIKBN   NOT =   1      )
                   MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
                   MOVE    WRK-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
                   ADD     1                   TO  LNK-WKSRYACT-MAX
                   MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC
                                                  (LNK-WKSRYACT-MAX)
                   MOVE    WRK-DRCD            TO  LNK-WKSRY-DRCD
                                                  (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科・保険組合せ毎に処方せんを印刷する
           MOVE    ZERO                TO  FLG-WKSRYEND
           PERFORM
                   UNTIL   FLG-WKSRYEND    =   1
               MOVE    SPACE               TO  WRK-SRYKA
               MOVE    SPACE               TO  WRK-HKNCOMBINUM
               MOVE    SPACE               TO  WRK-DRCD
               PERFORM VARYING     IDX-L   FROM    1   BY  1
                       UNTIL       IDX-L   >   LNK-WKSRYACT-MAX
                   IF      LNK-WKSRY-SYORI (IDX-L) =   SPACE
                       MOVE    LNK-WKSRY-SRYKA (IDX-L)
                                                   TO  WRK-SRYKA
                       MOVE    LNK-WKSRY-DRCD  (IDX-L)
                                                   TO  WRK-DRCD
                       MOVE    LNK-WKSRY-HKNCOMBI(IDX-L)
                                                   TO  WRK-HKNCOMBINUM
                       MOVE    LNK-WKSRYACT-MAX    TO  IDX-L
                   END-IF
               END-PERFORM
               IF      WRK-SRYKA           =   SPACE
                   MOVE    1               TO  FLG-WKSRYEND
               END-IF
      *        処方編集初期クリア処理（伝票毎）
               PERFORM 20031-WKSYOHOU-INIT-SEC
               PERFORM VARYING     IDX-L   FROM    1   BY  1
                       UNTIL      (IDX-L   >   LNK-WKSRYACT-MAX) OR
                                  (FLG-WKSRYEND    =   1   )
                   IF      LNK-WKSRY-SYORI (IDX-L) =   SPACE
                       IF     (LNK-WKSRY-SRYKA (IDX-L)
                                                   =   WRK-SRYKA)
                          AND (LNK-WKSRY-HKNCOMBI(IDX-L)
                                                   =   WRK-HKNCOMBINUM)
                          MOVE    LNK-WKSRYACT-REC (IDX-L)
                                                   TO  WKSRYACT-REC
      *                   対象チェック・編集
                          PERFORM 20032-WKSRYACT-CHK-SEC
                          MOVE    "1"              TO  LNK-WKSRY-SYORI
                                                                 (IDX-L)
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-HENSYU          =   1
                   PERFORM 20032-PRINT-HENSYU-SEC
                   IF      FLG-OK2             =   1
      *                前の剤の処方せん処理
                       PERFORM 20022-SYOHOU-HENSYU-SEC
                   END-IF
               END-IF
               IF      IDX             >   ZERO
      *            処方せん印刷
                   PERFORM 200212-PRINT-CHK-SEC
               END-IF
           END-PERFORM
           .
       2003-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方編集初期クリア処理
      *****************************************************************
       20031-WKSYOHOU-INIT-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HENSYU
           INITIALIZE                  WRK-SRYACT-GRP-AREA
      *    共通クリア
           PERFORM 200211-KYOTU-INIT-SEC
      *
           .
       20031-WKSYOHOU-INIT-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ編集処理
      *****************************************************************
       20032-WKSRYACT-CHK-SEC         SECTION.
      *
           IF      WKSRY-RENNUM        =   1
               IF      FLG-HENSYU          =   1
                   PERFORM 20032-PRINT-HENSYU-SEC
                   IF      FLG-OK2             =   1
      *                前の剤の処方せん処理
                       PERFORM 20022-SYOHOU-HENSYU-SEC
                   END-IF
                   MOVE    ZERO                TO  FLG-HENSYU
               END-IF
               INITIALIZE                  WRK-SRYACT-GRP-AREA
               PERFORM 20020-INIT-ZAINUM-SEC
               MOVE    1                   TO  FLG-HENSYU
      *        回数
               MOVE    WKSRY-ZAIKAIKEI     TO  WRK-KAISU
               MOVE    WKSRY-SRYSYUKBN     TO  WRK-SRYSYUKBN
               MOVE    WKSRY-SRYKBN        TO  WRK-SRYKBN
           END-IF
      *
      *    在宅自己注射はすべてで対象とする
           IF     (WKSRY-SRYKBN        =   "14"    )
               MOVE    WKSRY-YKZTEN        TO  WRK-YKZTEN
               MOVE    WKSRY-KIZAITEN      TO  WRK-KIZAITEN
           END-IF
      *    投薬対象
           IF      WKSRY-SRYKBN        =   "21"  OR
                                           "22"  OR
                                           "23"
               MOVE    WKSRY-ZAITENKEI     TO  WRK-ZAITENKEI
           END-IF
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )
               IF     (WKSRY-SRYCD (IDXA)      =   SPACE )
                  OR  (WKSRY-SRYCD (IDXA)(1:4) =   "0086")
                   CONTINUE
               ELSE
                   ADD     1                   TO  WRK-SRY-MAX
                   MOVE    WKSRY-SRYCD (IDXA)  TO  WRK-SRY-SRYCD
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-SRYSURYO (IDXA)
                                               TO  WRK-SRY-SRYSURYO
                                                         (WRK-SRY-MAX)
                   MOVE    WKSRY-SRYKAISU (IDXA)
                                               TO  WRK-SRY-SRYKAISU
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-AUTOKBN (IDXA)
                                               TO  WRK-SRY-AUTOKBN
                                                           (WRK-SRY-MAX)
                   MOVE    ZERO                TO  WRK-SRY-INPUTNUM
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-JIHIMONEY (IDXA)
                                               TO  WRK-SRY-JIHIMONEY
                                                           (WRK-SRY-MAX)
      *            名称
                   IF    ((WKSRY-SRYCD (IDXA)(1:1) =    "8")   OR
                          (WKSRY-SRYCD (IDXA)(1:3) =    "008")) AND
                          (WKSRY-INPUTCOMENT (IDXA)
                                               NOT =   SPACE)
                       MOVE    WKSRY-INPUTCOMENT (IDXA)
                                               TO  WRK-SRY-COM-NAME
                                                           (WRK-SRY-MAX)
                   END-IF
      *            用法コメント名称
                   IF     (WKSRY-SRYCD (IDXA)(1:3) =    "001")  AND
                          (WKSRY-INPUTCOMENT (IDXA)
                                               NOT =   SPACE)
                       MOVE    WKSRY-INPUTCOMENT (IDXA)
                                               TO  WRK-SRY-COM-NAME
                                                           (WRK-SRY-MAX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20032-WKSRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途終了剤毎の編集処理
      *****************************************************************
       20032-PRINT-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK2
      *    投薬で、点数がゼロのもを対象（院外）
           IF      ORCHC19-KBN         =   "4" 
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI       =   ZERO   )
                   MOVE    1               TO  FLG-OK2
               END-IF
      *        院内処方
               IF     (WRK-SRYSYUKBN       =   "211"
                                           OR  "221"
                                           OR  "231"
                                           OR  "291" )
                   MOVE    ZERO            TO  FLG-OK2
               END-IF
      *        院外処方
               IF     (WRK-SRYSYUKBN       =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292" )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *    投薬で、点数がゼロのもを対象（院内）
           IF      ORCHC19-KBN         =   "5"
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI       >   ZERO    )
                   MOVE    1               TO  FLG-OK2
               END-IF
      *        院内処方
               IF     (WRK-SRYSYUKBN       =   "211"
                                           OR  "221"
                                           OR  "231"
                                           OR  "291" )
                   MOVE    1               TO  FLG-OK2
               END-IF
      *        院外処方
               IF     (WRK-SRYSYUKBN       =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292" )
                   MOVE    ZERO            TO  FLG-OK2
               END-IF
           END-IF
      *
      *    在宅で薬剤・器材がある時、
           IF      WRK-SRYKBN          =   "14"
               IF     (WRK-SRYSYUKBN       =   "148"  OR
                                               "149" )  AND
                      (ORCHC19-KBN         =   "4"   )
                   MOVE    1               TO  FLG-OK2
               END-IF
               IF     (WRK-SRYSYUKBN   NOT =   "148"  AND
                                               "149" )  AND
                      (ORCHC19-KBN         =   "5"  )
      *            在宅で薬剤・器材がある時、
                   IF     (WRK-YKZTEN      >   ZERO )  OR
                          (WRK-KIZAITEN    >   ZERO )
                       MOVE    1               TO  FLG-OK2
                   END-IF
               END-IF
           END-IF
      *    処方のみで、院内処方印刷の時
           IF      WRK-SRYKBN          =   "21"  OR  "22" OR "23"
               IF     (WRK-SRYSYUKBN       =   "213"  OR
                                               "223"  OR
                                               "233")   AND
                      (ORCHC19-KBN         =   "5"   )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *    処方コメント
           IF      WRK-SRYKBN          =   "98"
               MOVE    1               TO  FLG-OK2
           END-IF
           .
       20032-PRINT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    前回処方（内容変更）処理
      *****************************************************************
       2004-HENKOU-SYORI-SEC         SECTION.
      *
      *    処方編集初期クリア処理
           PERFORM 200411-SYOHOU-SPAINIT-SEC
      *
           PERFORM VARYING     IDX-L   FROM    1   BY  1
                   UNTIL      (IDX-L   >   ORCHC19-ZAI-MAX)
               INITIALIZE                  WRK-SRYACT-GRP-AREA
               PERFORM 20020-INIT-ZAINUM-SEC
               MOVE    ORCHC19-ZAIKAIKEI(IDX-L)   TO  WRK-KAISU
               MOVE    ORCHC19-SRYSYUKBN(IDX-L)   TO  WRK-SRYSYUKBN
               MOVE    ORCHC19-SRYKBN   (IDX-L)   TO  WRK-SRYKBN
               PERFORM VARYING     IDX-L2  FROM    1   BY  1
                       UNTIL      (IDX-L2  >   50  )   OR
                                  (ORCHC19-SRYCD (IDX-L IDX-L2)
                                               =   SPACE )
                   IF      ORCHC19-SRYCD (IDX-L IDX-L2)(1:4)
                                               =   "0086"
                       CONTINUE
                   ELSE
                       PERFORM 20041-ORCHC19-HEN-SEC
                       MOVE    1                   TO  FLG-HENSYU
                   END-IF
               END-PERFORM
      *
               IF      FLG-HENSYU          =   1
      *            前の剤の処方せん処理
                   PERFORM 20022-SYOHOU-HENSYU-SEC
               END-IF
           END-PERFORM
      *    処方せん印刷
           PERFORM 200212-PRINT-CHK-SEC
           .
       2004-HENKOU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方編集初期クリア処理
      *****************************************************************
       200411-SYOHOU-SPAINIT-SEC          SECTION.
      *
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *
      *    共通クリア
           PERFORM 200211-KYOTU-INIT-SEC
      *
           .
       200411-SYOHOU-SPAINIT-EXT.
           EXIT.
      *****************************************************************
      *    前回処方編集処理
      *****************************************************************
       20041-ORCHC19-HEN-SEC         SECTION.
      *
           ADD     1                   TO  WRK-SRY-MAX
           MOVE    ORCHC19-SRYCD (IDX-L IDX-L2)
                                       TO  WRK-SRY-SRYCD  (WRK-SRY-MAX)
           MOVE    ORCHC19-SRYSURYO (IDX-L IDX-L2)
                                       TO  WRK-SRY-SRYSURYO
                                                          (WRK-SRY-MAX)
           MOVE    ORCHC19-SRYKAISU (IDX-L IDX-L2)
                                       TO  WRK-SRY-SRYKAISU
                                                          (WRK-SRY-MAX)
           IF    ((ORCHC19-SRYCD (IDX-L IDX-L2)(1:1)
                                       =    "8")   OR
                  (ORCHC19-SRYCD (IDX-L IDX-L2)(1:3)
                                       =    "008")  OR
                  (ORCHC19-SRYCD (IDX-L IDX-L2)(1:3)
                                       =    "001")) AND
                  (ORCHC19-INPUTCOMENT (IDX-L IDX-L2)
                                   NOT =   SPACE)
               MOVE    ORCHC19-INPUTCOMENT (IDX-L IDX-L2)
                                           TO  WRK-SRY-COM-NAME
                                                         (WRK-SRY-MAX)
           END-IF
      *
           .
       20041-ORCHC19-HEN-EXT.
           EXIT.
      *****************************************************************
      *    前回処方（選択なし）処理
      *****************************************************************
       2005-ZENKAI-SYORI-SEC         SECTION.
      *
      *    処方編集初期クリア処理
           PERFORM 200411-SYOHOU-SPAINIT-SEC
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ORCHC19-DENPNUM     TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *        受診履歴より編集
               PERFORM 20023-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    印刷判定
           PERFORM 200212-PRINT-CHK-SEC
      *
           .
       2005-ZENKAI-SYORI-EXT.
           EXIT.
      *---(04.00.00) LINE ADD START ------------------------------------
      *****************************************************************
      *    ＱＲデータ出力処理
      *****************************************************************
        600-QRDATA-SYORI-SEC           SECTION.
      *
      *    県番号
           MOVE    SYS-1001-PREFNUM    TO  ORCSQRCSV-PREFNUM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  ORCSQRCSV-HOSPCD
      *    医療機関種別
           MOVE    SYS-1001-HOSPSBT    TO  ORCSQRCSV-HOSPSBT
      *    医療機関名称
           MOVE    WRK-HOSPNAME        TO  ORCSQRCSV-HOSPNAME
      *    医療機関住所名称
           MOVE    WRK-HOSPADRS        TO  ORCSQRCSV-HOSPADRS
      *    医療機関郵便番号
           IF      SYS-1002-POST       NUMERIC
               MOVE    SYS-1002-POST(1:3)  TO  ORCSQRCSV-HOSPPOST(1:3)
               MOVE    "-"                 TO  ORCSQRCSV-HOSPPOST(4:1)
               MOVE    SYS-1002-POST(4:4)  TO  ORCSQRCSV-HOSPPOST(5:4)
           END-IF
      *    医療機関電話
           MOVE    SYS-1002-TEL        TO  ORCSQRCSV-HOSPTEL
      *    診療科
           MOVE    SYS-1005-RECESRYKA  TO  ORCSQRCSV-RECESRYKA
           MOVE    SYS-1005-SRYKANAME  TO  ORCSQRCSV-SRYKANAME
      *    医師情報
           MOVE    SYS-1010-KANANAME   TO  ORCSQRCSV-DR-KANANAME
           MOVE    SYS-1010-NAME       TO  ORCSQRCSV-DR-NAME
      *    患者情報
           MOVE    PTINF-NAME          TO  ORCSQRCSV-NAME
           MOVE    PTINF-KANANAME      TO  ORCSQRCSV-KANANAME
           MOVE    PTINF-SEX           TO  ORCSQRCSV-SEX
           MOVE    PTINF-BIRTHDAY      TO  ORCSQRCSV-BIRTHDAY
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO    ORCSQRCSV-ADRS
           END-STRING
           MOVE    PTINF-HOME-TEL1     TO  ORCSQRCSV-TEL
      *    患者保険情報
           MOVE    WRK-HKNCOMBINUM     TO  ORCSQRCSV-HKNCOMBINUM 
      *    対象公費編集
           MOVE    ZERO                TO  IDX-K2
           PERFORM VARYING    IDX-K    FROM    1   BY  1
                   UNTIL     (IDX-K    >   4   )
                         OR  (COMB-KOHID (IDX-K)   =   ZERO )
               PERFORM 6001-QRKOH-HENSYU-SEC
           END-PERFORM
      *    備考編集
           PERFORM 6002-QRBIKOU-HENSYU-SEC
      *    麻毒フラグ
           MOVE    FLG-MAYAKU          TO  ORCSQRCSV-MAYAKU
      *    一部負担割合備考
           MOVE    WRK-BIKOU-FTN       TO  ORCSQRCSV-BIKO-FTN
      *    ファイル名組み立て項目
           MOVE    WRK-SYS-TIME        TO  ORCSQRCSV-SYSTIME
      *
           CALL    "ORCSQRCSV"         USING
                                       SPA-AREA
                                       ORCSQRCSVAREA
      *
           .
       600-QRDATA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ公費情報変数処理
      *****************************************************************
       6001-QRKOH-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KOHERR
      *    地方公費表示しない
           IF      PARA-KOHKBN         =   "1"
               IF      COMB-KOHHKNNUM (IDX-K)
                                       >=  "100"
                                   AND <=  "899"
                   MOVE    1               TO  FLG-KOHERR
               END-IF
           END-IF
      *
           IF      FLG-KOHERR          =   ZERO
               ADD     1               TO  IDX-K2
               MOVE    COMB-KOHID (IDX-K)
                                       TO  ORCSQRCSV-KOHID    (IDX-K2)
               MOVE    COMB-KOHHKNNUM (IDX-K)
                                       TO  ORCSQRCSV-KOHHKNNUM(IDX-K2)
           END-IF
           .
       6001-QRKOH-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ用備考編集処理
      *****************************************************************
       6002-QRBIKOU-HENSYU-SEC           SECTION.
      *
      *    備考の最初に地方公費を編集
           PERFORM VARYING     IDX-K3  FROM    4   BY  -1
                   UNTIL      (IDX-K3      <   1   )
               IF      WRK-HEN-KOH (IDX-K3)    NOT =   SPACE
                   PERFORM VARYING     IDX-QRB FROM    10  BY  -1
                           UNTIL      (IDX-QRB     <   1   )
                       IF      IDX-QRB             =   1
                           MOVE    WRK-HEN-KOH (IDX-K3)
                                                   TO  ORCSQRCSV-BIKOU
                                                          (IDX-QRB)
                       ELSE
                           MOVE    ORCSQRCSV-BIKOU (IDX-QRB - 1)
                                                   TO  ORCSQRCSV-BIKOU
                                                          (IDX-QRB)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       6002-QRBIKOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    医療機関編集情報処理
      *****************************************************************
        900-SYS1900-SYORI-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(9)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-SYS1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-SYS1902-READ-SEC
               END-IF
           END-IF
           .
      *
        900-SYS1900-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-SYS1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(9)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           END-IF
           .
      *
       900-SYS1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-SYS1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(9)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1      TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2      TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL        TO  WRK-HOSPTEL
           END-IF
           .
      *
       900-SYS1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *---(03.04.01) LINE ADD END   ------------------------------------
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
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
      *    患者マスター読み込み
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
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
     *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
     *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
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
      *
      *---(03.04.00) LINE ADD START ------------------------------------
      *    点数附加情報検索
           IF     (TNS-SRYCD(1:1)      =   "6" ) AND
                  (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
      *    一般名記載あり
           IF      TNSP-IPN-KISAIKBN   =   1
               PERFORM 9102-GENERICNAME-READ-SEC
           END-IF
      *---(03.04.00) LINE ADD END   ------------------------------------
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *---(03.04.00) LINE ADD START ------------------------------------
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
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
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9102-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
           MOVE    TNS-YAKKAKJNCD(1:9) TO  GENERIC-YAKKAKJNCD
      **** MOVE    SPA-HOSPNUM         TO  GENERIC-HOSPNUM
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *---(03.04.00) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
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
       900-SRYACT-READ-EXT.
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
      *---(01.00.07) LINE ADD START ------------------------------------
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC           SECTION.
      *
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
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
