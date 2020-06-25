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
       PROGRAM-ID.             A21000H02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 処方箋　印刷（Ａ５）
      *                      Ｈ１４．１０対応分（ORCHC19のCOPY）
      *                      MONPE で作成
      *  管理者            : 
      *  作成日付    作業者      記述
      *  03/03/31    @@@@−半田  新規作成(1.0.6カスタマイズ)
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/03/10  用法コードコメント対応
      *  01.00.03    @@@@-門間    12/04/09  Ｈ２４年４月改正対応
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "A21000H02.INC".
      *
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
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
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SOURYOHEN       PIC 9(01).
           03  FLG-GENERICNAME     PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-ZAITAKU-ARI     PIC 9(01).
      *
           03  FLG-INNAI           PIC 9(01).
      *
           03  FLG-SYOHOU          PIC 9(01).
      *
           03  FLG-PRINT           PIC 9(01).
      *    麻薬区分
           03  FLG-MAYAKU          PIC 9(01).
      *
           03  FLG-OK2             PIC 9(01).
      *    老人フラグ
           03  FLG-RJNHKN          PIC 9(01).
      *    対象外フラグ
           03  FLG-TAIGAI          PIC 9(01).
      *
           03  FLG-WKSRYEND        PIC 9(01).
           03  FLG-INGAI           PIC 9(01).
           03  FLG-HENSYU              PIC 9(01).
      *    後発医薬品変更可
           03  FLG-KOUHATU-HEN         PIC 9(01).
           03  FLG-KOUHATU-CHK         PIC 9(01).
           03  FLG-KOUHATU             PIC 9(01).
           03  FLG-KOUHATU-1           PIC 9(01).
           03  FLG-KOUHATU-1-CHK       PIC 9(01).
           03  FLG-NAME                PIC 9(01).
      *    後発医薬品変更不可あり
           03  FLG-KOUHATUFUKA-IN      PIC 9(01).
           03  FLG-KOUHATUFUKA-ARI     PIC 9(01).
      *    後発品・先発品あり
           03  FLG-KOUHATU-ARI         PIC 9(01).
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
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX5                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
           03  KETA                PIC 9(04).
      *
           03  IDX-L               PIC 9(04).
           03  IDX-L2              PIC 9(04).
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
           03  WRK-SRYKA           PIC  X(02).
      *
           03  WRK-DRCD            PIC  X(05).
      *
           03  WRK-DENPNUM         PIC  9(07).
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(08).
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
      *
           03  WRK-FTNSNO          PIC X(16).
           03  WRK-JKYSNO          PIC X(20).
           03  WRK-FTNJA1          PIC X(16).
           03  WRK-JKYSNUM1        PIC X(20).
           03  WRK-FTNJA2          PIC X(16).
           03  WRK-JKYSNUM2        PIC X(20).
           03  WRK-JKYSNUMB        PIC X(20)  OCCURS 2.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-SURYO-N         PIC X(18).
           03  WRK-SURYO-N22       PIC X(22).
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
           03  WRK-TANINAME        PIC X(04).
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-KAISU           PIC 9(05).
      *
           03  WRK-COM-NAME        PIC X(200).
           03  WRK-TNS-NAME        PIC X(200).
           03  WRK-NAME            PIC X(200).
      *
           03  WRK-ZAITENKEI        PIC 9(07).
           03  WRK-YKZTEN          PIC 9(07).
           03  WRK-KIZAITEN        PIC 9(07).
      *    用法コメント名称
           03  WRK-YOH-NAME            PIC X(200).
      *
      *    文字変換
           03  WRK-ZENKAKU-G           PIC X(100).
           03  WRK-MOJI                PIC X(100).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-FTNKBN              PIC X(20).
           03  WRK-KIGONUM             PIC X(80).
           03  WRK-NUM                 PIC X(80).
      *
           03  WRK-HEN-KIGONUM         PIC X(80).
      *
      *    麻薬編集
           03  WRK-HENSYU-AREA.
               05  WRK-HENSYU          PIC X(86)   OCCURS  3.
           03  IDX-H                   PIC 9(04).
           03  WRK-ADRS                PIC X(200).
      *    保険組合せ
           03  WRK-HKNCOMBINUM         PIC X(04).
      *
      *    負担割合
           03  WRK-BIKOU               PIC X(86).
      *
           03  WRK-ORC-DBPATH          PIC X(64).
           03  WRK-HOSPNAME            PIC X(80).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(40).
               05  WRK-HOSPNAME2       PIC X(40).
           03  WRK-HOSPADRS            PIC X(80).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(40).
               05  WRK-HOSPADRS2       PIC X(40).
           03  WRK-HOSPTEL             PIC X(15).
           03  WRK-MSG             PIC X(200).
           03  WRK-SYS-TIME            PIC 9(08).
           03  WRK-FILE-CNT2           PIC 9(02).
           03  WRK-PREFNUM             PIC X(04).
           03  WRK-TENHYOKBN           PIC X(02).
           03  WRK-HOSPCD              PIC X(14).
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
                   07  WRK-MEISYOU2        PIC X(44).
                   07  WRK-FIL22           PIC X(22).
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
      *    薬剤編集
       01  WRK-MEISAI-AREA.
           03  WRK-M-SRYCD               PIC X(09).
           03  WRK-M-NAME                PIC X(100).
           03  WRK-M-KETA                PIC 9(03).
           03  WRK-M-SURYOU              PIC X(18).
           03  WRK-M-TANI                PIC X(12).
           03  WRK-KOU-KETA2             PIC 9(03).
           03  WRK-KOU-SRYCD             PIC 9(09).
      *
           03  WRK-KOU-NAME            PIC X(100).
           03  WRK-KOU-KETA            PIC 9(03).
      *
      *    処方内容
       01  WRK-SYOHOU-AREA.
           03  WRK-SYOHOU-PAGE         OCCURS   10.
               05  WRK-SYOHOU-TBL          OCCURS   20.
                   07  WRK-NAIYOU          PIC X(86).
      *
           03  WRK-PAGE-MAX                PIC 9(03).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    処方せん明細行
       01  LINE-MAX                PIC 9(03)  VALUE 20.
      *
      *    免除判定領域
       01  WRK-MENJYO-ARAE.
           03  WRK-MENJYO-FLG          PIC 9(01).
      *    コメント備考
       01  WRK-BIKOU-AREA.
           03  WRK-BIKOU-G.
             05  WRK-BIKOU-NAME          PIC X(172)
                                   OCCURS  9.
           03  WRK-BIKOU-G2  REDEFINES  WRK-BIKOU-G.
             05  WRK-BIKOU-NAME2         PIC X(86)
                                         OCCURS 18.
           03  IDX-BIKOU                 PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
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
      *
      *    出力先プリンタ名割り当て情報（入院）
           COPY  "CPSK1034.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
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
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ワーク診療行為マスタ
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
      *--- 20110107 DELETE START ---------------------------------------
      *     03  LNK-WKSRY-DRCD          PIC X(05).
      *--- 20110107 DELETE END   ---------------------------------------
           03  LNK-WKSRY-SYORI         PIC X(01).
      *
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *--- 20110107 DELETE START ---------------------------------------
      **   免除判定サブ
      *     COPY    "CPORCSMENJYO.INC".
      *--- 20110107 DELETE END   ---------------------------------------
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
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
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
      *    平成２４年４月から別ＰＧとする
           IF      SPA-SRYYMD          >=  "20120401"
               CALL    "A21000H02V02"      USING   SPA-AREA
                                                   ORCHC19AREA
               GO      TO      000-PROC-EXT
           END-IF
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-MENJYO-ARAE
      *
      *    ファイル名編集処理
           PERFORM 1001-HC02-FILE-SEC
      *
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
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
      *--- 20110107 CHANGE START ---------------------------------------
      *     MOVE    "HCM19"             TO  HC01PARA-FORM-ID
           MOVE    "HC19"              TO  HC01PARA-FORM-ID
      *--- 20110107 CHANGE END   ---------------------------------------
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    WRK-SYS-TIME        TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT               PRT-FILE
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
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
      *
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
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *　　印刷用データ出力
            WRITE  HCM19
      *
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
           IF    SPA-SRYYMD  <  "20100401"
             MOVE   "A21000H02.red"     TO  MKPRT-DIA
           ELSE
             MOVE   "A21000H02V02.red"     TO  MKPRT-DIA
           END-IF
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(2)   TO  MKPRT-PRTNM
      *    退院時院外処方せん（入院）
            IF    (ORCHC19-KBN         =   "N"       )  AND
                  (SYS-1034-PRTNM(12)  NOT =   SPACE )
                MOVE   SYS-1034-PRTNM(12)  TO  MKPRT-PRTNM
            END-IF
            MOVE  "処方せん"     TO MKPRT-INFO
            CALL   "ORCSMKPRTSITE1"     USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-EXT.
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
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *    システム管理検索
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
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
      *    システム管理検索
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
      *
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
      *    県番号
           MOVE    SYS-1001-PREFNUM    TO  WRK-MOJI
           MOVE    2                   TO  WRK-MOJISU
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:4)  TO  WRK-PREFNUM
      *    点数表区分
           MOVE    SYS-1001-TENHYOKBN  TO  WRK-MOJI
           MOVE    1                   TO  WRK-MOJISU
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:2)  TO  WRK-TENHYOKBN
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  WRK-MOJI
           MOVE    7                   TO  WRK-MOJISU
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:14) TO  WRK-HOSPCD
      *    医療機関編集情報
           PERFORM 900-SYS1900-SYORI-SEC
      *
           .
       100-SYORI-EXT.
           EXIT.
      *
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
      *
           EVALUATE    ORCHC19-KBN
               WHEN    "1"
      *            前回処方印刷
                   IF      ORCHC19-DENPNUM     =   ZERO
                       PERFORM 2001-RENNUM-SET-SEC
                   ELSE
                       PERFORM 2005-ZENKAI-SYORI-SEC
                   END-IF
               WHEN    "2"
      *            見出しのみ
                   INITIALIZE                  WRK-SYOHOU-AREA
                   MOVE    1                   TO  WRK-PAGE-MAX
                   MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
                   PERFORM 400-PRINT-SYORI-SEC
               WHEN    "4"
               WHEN    "5"
      *            中途終了時、発行
                   PERFORM 2003-WKSRYACT-SYORI-SEC
               WHEN    "A"
      *            前回処方（内容変更）
               WHEN    "D"
      *            前回処方（日数ゼロ）（選択）
               WHEN    "N"
      *            退院時処方せん
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
           MOVE    "key26"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key26"              TO  MCP-PATHNAME
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
               MOVE    "key26"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
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
      *
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    1                   TO  WRK-PAGE-MAX
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  FLG-MAYAKU
           MOVE    ZERO                TO  FLG-KOUHATU-HEN
           MOVE    ZERO                TO  FLG-KOUHATU-CHK
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-1
           MOVE    ZERO                TO  FLG-KOUHATU-1-CHK
      *
           INITIALIZE                  WRK-BIKOU-AREA
           .
       200211-SYOHOU-INIT-EXT.
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
                      (IDX             =   20  )
                   MOVE    "　　　　　　以下余白"
                                       TO  WRK-NAIYOU (WRK-PAGE-MAX IDX)
               END-IF
               ADD     1               TO  IDX
               IF      IDX             <=  20
                   MOVE    "　　　　　　以下余白"
                                       TO  WRK-NAIYOU (WRK-PAGE-MAX IDX)
               END-IF
               MOVE    1               TO  FLG-PRINT
           END-IF
      *
      *    総頁数から印刷を判定する
           IF      WRK-PAGE-MAX        >   ZERO
      *        麻薬編集
               IF      FLG-MAYAKU          =   1
                   PERFORM 2003-MAYAKU-HEN-SEC
               END-IF
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
      *
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
           MOVE    ZERO                TO  FLG-SYORI
      *
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
      *
                   IF  SPA-HKNCOMBINUM     NOT =   SPACE
                       MOVE   SPA-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   END-IF
      *
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
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key7"              TO  MCP-PATHNAME
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
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
      *    各ＤＢ検索
           PERFORM 4001-DB-KENSAKU-SEC
      *
      *        麻薬編集
               IF      FLG-MAYAKU          =   1
                   PERFORM 2003-MAYAKU-HEN-SEC
               END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HCM19-HEN-SEC
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
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
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
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
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
           MOVE    SPA-HOSPNUM          TO  PTHKN-HOSPNUM
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
           MOVE    16                   TO  WRK-MOJISU
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
      *    公費情報１
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOH1ID         TO  PTKOH-KOHID
           PERFORM 940-PTKOHINF-READ-SEC
           IF      FLG-PTKOHINF        =   ZERO
               PERFORM 3102-PTKOHINF-HEN-SEC
               IF      PTKOH-KOHNUM   NOT =   "141"
                 AND   PTKOH-KOHNUM   NOT =   "143"
                 AND   PTKOH-KOHNUM   NOT =   "144"
                 AND   PTKOH-KOHNUM   NOT =   "145"
                 AND   PTKOH-KOHNUM   NOT =   "146"
                 AND   PTKOH-KOHNUM   NOT =   "147"
                 AND   PTKOH-KOHNUM   NOT =   "199"
                       MOVE    WRK-FTNSNO  TO  WRK-FTNJA1
                       MOVE    WRK-JKYSNO  TO  WRK-JKYSNUM1
               ELSE
                       PERFORM  VARYING    IDX5  FROM   1  BY  1
                                UNTIL      IDX5  >      2
                        IF  WRK-JKYSNUMB(IDX5) = SPACE
                          MOVE  PTKOH-JKYSNUM TO  WRK-JKYSNUMB(IDX5)
                          MOVE  3             TO  IDX5
                        END-IF
                       END-PERFORM
               END-IF
           ELSE
               MOVE    SPACE           TO  WRK-FTNJA1
               MOVE    SPACE           TO  WRK-JKYSNUM1
           END-IF
      *
      *    公費情報２
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOH2ID         TO  PTKOH-KOHID
           PERFORM 940-PTKOHINF-READ-SEC
           IF      FLG-PTKOHINF          =   ZERO
               PERFORM 3102-PTKOHINF-HEN-SEC
               IF      PTKOH-KOHNUM   NOT =   "141"
                 AND   PTKOH-KOHNUM   NOT =   "143"
                 AND   PTKOH-KOHNUM   NOT =   "144"
                 AND   PTKOH-KOHNUM   NOT =   "145"
                 AND   PTKOH-KOHNUM   NOT =   "146"
                 AND   PTKOH-KOHNUM   NOT =   "147"
                 AND   PTKOH-KOHNUM   NOT =   "199"
                       IF    WRK-FTNJA1    =   SPACE
                         AND WRK-JKYSNUM1  =   SPACE
                             MOVE    WRK-FTNSNO  TO  WRK-FTNJA1
                             MOVE    WRK-JKYSNO  TO  WRK-JKYSNUM1
                             MOVE    SPACE           TO  WRK-FTNJA2
                             MOVE    SPACE           TO  WRK-JKYSNUM2
                       ELSE
                             MOVE    WRK-FTNSNO  TO  WRK-FTNJA2
                             MOVE    WRK-JKYSNO  TO  WRK-JKYSNUM2
                       END-IF
               ELSE
                       PERFORM  VARYING    IDX5  FROM   1  BY  1
                                UNTIL      IDX5  >      2
                        IF  WRK-JKYSNUMB(IDX5) = SPACE
                          MOVE  PTKOH-JKYSNUM TO  WRK-JKYSNUMB(IDX5)
                          MOVE  3             TO  IDX5
                        END-IF
                       END-PERFORM
               END-IF
           ELSE
               MOVE    SPACE           TO  WRK-FTNJA2
               MOVE    SPACE           TO  WRK-JKYSNUM2
           END-IF
      *
      *    公費情報３
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOH3ID         TO  PTKOH-KOHID
           PERFORM 940-PTKOHINF-READ-SEC
           IF      FLG-PTKOHINF          =   ZERO
               PERFORM 3102-PTKOHINF-HEN-SEC
               IF      PTKOH-KOHNUM   NOT =   "141"
                 AND   PTKOH-KOHNUM   NOT =   "143"
                 AND   PTKOH-KOHNUM   NOT =   "144"
                 AND   PTKOH-KOHNUM   NOT =   "145"
                 AND   PTKOH-KOHNUM   NOT =   "146"
                 AND   PTKOH-KOHNUM   NOT =   "147"
                 AND   PTKOH-KOHNUM   NOT =   "199"
                       IF    WRK-FTNJA1    =   SPACE
                         AND WRK-JKYSNUM1  =   SPACE
                             MOVE    WRK-FTNSNO  TO  WRK-FTNJA1
                             MOVE    WRK-JKYSNO  TO  WRK-JKYSNUM1
                       ELSE
                         IF    WRK-FTNJA2    =   SPACE
                           AND WRK-JKYSNUM2  =   SPACE
                             MOVE    WRK-FTNSNO  TO  WRK-FTNJA2
                             MOVE    WRK-JKYSNO  TO  WRK-JKYSNUM2
                         END-IF
                       END-IF
               ELSE
                       PERFORM  VARYING    IDX5  FROM   1  BY  1
                                UNTIL      IDX5  >      2
                        IF  WRK-JKYSNUMB(IDX5) = SPACE
                          MOVE  PTKOH-JKYSNUM TO  WRK-JKYSNUMB(IDX5)
                          MOVE  3             TO  IDX5
                        END-IF
                       END-PERFORM
               END-IF
           END-IF
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
           IF      COMB-HKNNUM         =  "975"
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNJA-G
           END-IF
      *
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
      *
               END-IF
           END-IF
      *
      *    ３歳未満・老人判定
           MOVE    SPACE               TO  WRK-BIKOU
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    WRK-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF SPA-SRYYMD  <  "20080401"
      *    ３歳未満
               IF      AGECHK-NENREI       <   03
                   MOVE    "（３歳）"     TO  WRK-BIKOU
               END-IF
           ELSE
      *    就学前
               IF      AGECHK-SYUGAKUKBN   =  1
                   MOVE    "（６歳）"   TO   WRK-BIKOU
               END-IF
           END-IF
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
      *                        ３割
                                   MOVE    3           TO  FLG-RJNHKN
                           END-EVALUATE
                       END-IF
               END-EVALUATE
           END-IF
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
      *    老人公費（保険より公費の割合を優先する）
           EVALUATE    TRUE
               WHEN    COMB-KOH1HKNNUM     =  "027"
                   EVALUATE    COMB-KOH1PAYKBN
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
               WHEN    COMB-KOH2HKNNUM     =  "027"
                   EVALUATE    COMB-KOH2PAYKBN
                       WHEN    "01"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
                           MOVE    2           TO  FLG-RJNHKN
                       WHEN    "03"
      *                        ３割
                           MOVE    3           TO  FLG-RJNHKN
                   END-EVALUATE
               WHEN    COMB-KOH3HKNNUM     =  "027"
                   EVALUATE    COMB-KOH3PAYKBN
                       WHEN    "01"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
                           MOVE    2           TO  FLG-RJNHKN
                       WHEN    "03"
      *                        ３割
                           MOVE    3           TO  FLG-RJNHKN
                   END-EVALUATE
               WHEN    COMB-KOH4HKNNUM     =  "027"
                   EVALUATE    COMB-KOH4PAYKBN
                       WHEN    "01"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
                           MOVE    2           TO  FLG-RJNHKN
                       WHEN    "03"
      *                        ３割
                           MOVE    3           TO  FLG-RJNHKN
                   END-EVALUATE
           END-EVALUATE
      *
           EVALUATE    FLG-RJNHKN
               WHEN    1
                   IF      SPA-SRYYMD          <   "20080401"
                       MOVE    "（高９）"          TO  WRK-BIKOU
                   ELSE
                       MOVE    "（高一）"          TO  WRK-BIKOU
                   END-IF
               WHEN    2
                   MOVE    "（高８）"          TO  WRK-BIKOU
               WHEN    3
                   MOVE    "（高７）"          TO  WRK-BIKOU
           END-EVALUATE
      *
           .
       4001-DB-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       3102-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-FTNSNO
           MOVE    SPACE               TO  WRK-JKYSNO
      *    負担者番号
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
           END-IF
      *
           .
       3102-PTKOHINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HCM19-HEN-SEC                SECTION.
      *
           INITIALIZE                  HCM19
      *
      *    患者カナ名称
           MOVE    PTINF-KANANAME      TO  HCM19-KANANAME
      *    患者名称
           MOVE    PTINF-NAME          TO  HCM19-NAME
      *
           EVALUATE   SPA-SEX
               WHEN   "男"
                      MOVE    "○"     TO  HCM19-SEX1
               WHEN   "女"
                      MOVE    "○"     TO  HCM19-SEX2
           END-EVALUATE
      *    生年月日
           MOVE    SPA-BIRTHDAY        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  WRK-HENYMDG(1:4)
               WHEN  "明治"
                     MOVE    "○"      TO  HCM19-BIRTH-G1
               WHEN  "大正"
                     MOVE    "○"      TO  HCM19-BIRTH-G2
               WHEN  "昭和"
                     MOVE    "○"      TO  HCM19-BIRTH-G3
               WHEN  "平成"
                     MOVE    "○"      TO  HCM19-BIRTH-G4
           END-EVALUATE
           MOVE    WRK-HENYMDG(5:4)    TO  HCM19-BIRTH-YY
           MOVE    WRK-HENYMDG(11:4)   TO  HCM19-BIRTH-MM
           MOVE    WRK-HENYMDG(17:4)   TO  HCM19-BIRTH-DD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM19-HOSPADRS
           INSPECT HCM19-HOSPADRS      REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM19-HOSPNAME
           INSPECT HCM19-HOSPNAME      REPLACING ALL "  " BY "　"
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCM19-HOSPTEL
      *    県番号
           MOVE    WRK-PREFNUM         TO  HCM19-PREFNUM
      *    点数表区分
           MOVE    WRK-TENHYOKBN       TO  HCM19-TENHYOKBN
      *    医療機関コード
           MOVE    WRK-HOSPCD          TO  HCM19-HOSPCD
      *
      *    ドクター
           MOVE    SYS-1010-NAME       TO  HCM19-DRNAME
      *
      *    保険者情報
      *    保険者番号
           MOVE    WRK-HKNJA-G         TO  HCM19-HKNJA
      *
      *    記号・番号
           MOVE    WRK-HEN-KIGONUM     TO  HCM19-KIGONUM
      *    後期高齢者は番号のみ
           IF    COMB-HKNNUM      =     "039"
               MOVE   WRK-NUM     TO     HCM19-KIGONUM
           ELSE
      *    記号・番号で１６文字以上は別行に編集
               IF     WRK-HEN-KIGONUM(33:)   NOT =   SPACE
                   MOVE    SPACE         TO     HCM19-KIGONUM
                   MOVE    WRK-KIGONUM   TO     HCM19-KIGO
                   MOVE    WRK-NUM       TO     HCM19-NUM
               END-IF
           END-IF
      *  　 アスタリスクによる塗りつぶし対応
           EVALUATE    PTHKN-HONKZKKBN
               WHEN    "1"
                   MOVE    "＊＊＊＊＊＊"     TO  HCM19-HONKZKKBN2
               WHEN    "2"
                   MOVE    "＊＊＊＊＊＊"     TO  HCM19-HONKZKKBN1
           END-EVALUATE
      *
      *    交付年月日
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(5:4)    TO  HCM19-KOFYY
           MOVE    WRK-HENYMDG(11:4)   TO  HCM19-KOFMM
           MOVE    WRK-HENYMDG(17:4)   TO  HCM19-KOFDD
      *
      *    公費負担者番号
           MOVE    WRK-FTNJA1          TO  HCM19-FTNJA
      *    ７文字以上判定
           IF      WRK-JKYSNUM1(15:)   =   SPACE
               MOVE    WRK-JKYSNUM1        TO  HCM19-JKYSNUM
           ELSE
               MOVE    WRK-JKYSNUM1        TO  HCM19-JKYSNUMN
           END-IF
      *
           MOVE    WRK-FTNJA2          TO  HCM19-FTNJA2
      *    ７文字以上判定
           IF      WRK-JKYSNUM2(15:)   =   SPACE
               MOVE    WRK-JKYSNUM2        TO  HCM19-JKYSNUM2
           ELSE
               MOVE    WRK-JKYSNUM2        TO  HCM19-JKYSNUM2N
           END-IF
      *
      *    岐阜県地方公費 受給者番号(備考欄)
           PERFORM VARYING  IDX5    FROM  1   BY  1
                   UNTIL    IDX5    >     2
                   MOVE  WRK-JKYSNUMB(IDX5)  TO  HCM19-JKYSNUMB(IDX5)
           END-PERFORM
      *
      *    処方内容
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   20
               MOVE    WRK-NAIYOU (IDX IDX2)   TO  HCM19-NAIYO (IDX2)
           END-PERFORM
      *
      *備考欄
      *    ３歳未満・老人負担割合
           MOVE    ZERO                TO  IDX-B
           IF      WRK-BIKOU       NOT =   SPACE
               ADD     1               TO  IDX-B
               MOVE    WRK-BIKOU       TO  HCM19-BIKOH (IDX-B)
           END-IF
      *
      *    麻薬編集
           IF      FLG-MAYAKU          =   1
               PERFORM 31020-MAYAKU-HEN-SEC
           END-IF
      *
           IF      IDX-BIKOU   > ZERO
             PERFORM  3009-COMMENT-HEN-SEC
           END-IF
      *
           IF   SPA-SRYYMD  <  "20080401"
      *    後発医薬品変更可
             IF   ((FLG-KOUHATU-HEN  =  1 )
                 OR  (SYS-1030-KOUHATUKA  =  "1"  OR  "2"))
                 AND  (FLG-KOUHATU-CHK  =  1 )
      *    ドクター
               MOVE    SYS-1010-NAME       TO   HCM19-HKNINAME
             END-IF
      *    後発医薬品の後発医薬品への変更可
             IF  ( ( FLG-KOUHATU-HEN  =   1)
                 OR   ( SYS-1030-KOUHATUKA  =  "2" ) )
                 AND  ( FLG-KOUHATU-1-CHK  =  1 )
      *    ドクター
               MOVE    SYS-1010-NAME       TO   HCM19-HKNINAME
             END-IF
           ELSE
      *    変更可の署名あり、変更不可の入力がある時
             IF  (FLG-KOUHATU-HEN  =  1 )
                 OR  (FLG-KOUHATUFUKA-ARI  =  1 )
               CONTINUE
             ELSE
      *    変更不可の署名
               IF  ( FLG-KOUHATUFUKA-IN  =  1 )
                   OR  ( SYS-1030-KOUHATUKA  =  "0" )
                 MOVE    SYS-1010-NAME     TO   HCM19-HKNINAME
               END-IF
             END-IF
           END-IF
      *
           .
       300-HCM19-HEN-EXT.
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
      *        麻薬編集編集処理
      *****************************************************************
       31020-MAYAKU-HEN-SEC      SECTION.
      *
         PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL      (IDX-H   >   3   )  OR
                              (IDX-B   >=  4   )
               IF      WRK-HENSYU (IDX-H)  NOT =   SPACE
                   ADD     1                   TO  IDX-B
                   MOVE    WRK-HENSYU (IDX-H)  TO  HCM19-BIKOH (IDX-B)
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
       3009-COMMENT-HEN-SEC     SECTION.
      *
           PERFORM  VARYING  IDX-BIKOU  FROM 1 BY 1
                    UNTIL   (IDX-BIKOU  > 18 ) OR
                            (IDX-B  >= 4)
             IF  WRK-BIKOU-NAME2(IDX-BIKOU)  NOT  =  SPACE
               ADD   1                           TO  IDX-B
               MOVE  WRK-BIKOU-NAME2(IDX-BIKOU)  TO  HCM19-BIKOH(IDX-B)
             END-IF
           END-PERFORM
      *
           .
       3009-COMMENT-HEN-EXT.
           EXIT.
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
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  FLG-ZAITAKU-ARI
           INITIALIZE                  WRK-MEISAI-AREA
           MOVE    ZERO                TO  FLG-HENSYU
           MOVE    ZERO                TO  FLG-SOURYOHEN
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
               MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
      *
               MOVE    ZERO                TO  FLG-TAIGAI
      *
      *        在宅の時、薬剤の有無をチェックする
               IF     (SRY-SRYKBN          =   "14"  )  AND
                      (FLG-ZAITAKU         =   ZERO  )
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )   OR
                                      (SRY-SRYCD (IDXA)    =   SPACE)
                       IF     (SRY-SRYCD (IDXA)(1:1)       =   "6"  OR
                                                               "7" )  OR
                              (SRY-SRYCD (IDXA)(1:3)       =   "059" )
                           MOVE    1               TO  FLG-ZAITAKU-ARI
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        在宅で薬剤・器材がある時、
               MOVE    ZERO                TO  FLG-ZAITAKU
      *
               IF     (SRY-SRYKBN          =   "14" ) 
                   IF      ORCHC19-KBN         =   "3"
                                               OR  "C"
      *                院内
                       IF     (SRY-SRYSYUKBN       =   "148"  OR
                                                       "149" )  OR
                              (FLG-ZAITAKU-ARI     =   ZERO )
                           MOVE    1               TO  FLG-TAIGAI
                       ELSE
                           MOVE    1               TO  FLG-ZAITAKU
                       END-IF
                   ELSE
      *                院外
                       IF     (SRY-SRYSYUKBN       =   "148"  OR
                                                       "149" )
                           MOVE    1               TO  FLG-ZAITAKU
                       ELSE
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   END-IF
               END-IF
      *
      *        処方のみで、院内処方印刷の時
               MOVE    ZERO                TO  FLG-INNAI
               IF     (SRY-SRYSYUKBN       =   "213"  OR
                                               "223"  OR
                                               "233")
                   IF      ORCHC19-KBN         =   "3"
                                               OR  "C"
                       MOVE    1               TO  FLG-INNAI
                   ELSE
                       MOVE    1               TO  FLG-TAIGAI
                   END-IF
               END-IF
      *
      *        薬剤、在宅薬剤を対象とする
               IF     (SRY-SRYKBN          =   "21"   OR  "22" OR "23"
                                           OR  "14"   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-TAIGAI
               END-IF
      *
      *        在宅薬剤がないとき対象外
               IF     (SRY-SRYKBN          =   "14" )  AND
                      (SRY-SRYSYUKBN   NOT =   "148"  AND
                                               "149" ) AND
                      (FLG-ZAITAKU-ARI     =   ZERO)
                   MOVE    1               TO  FLG-TAIGAI
               END-IF
      *
      *        薬剤、在宅薬剤を対象とする
               IF      FLG-TAIGAI          =   ZERO
      *            診療会計より、点数チェック
                   PERFORM 20021-SRYACCT-SYORI-SEC
               END-IF
               IF   FLG-TAIGAI  =  ZERO
                   IF     (ORCHC19-KBN         =   "3"
                                               OR  "C"  )
      *                院内判定
                       IF      ACCT-ZAITEN    NOT =   ZERO
                           MOVE    1               TO  FLG-INNAI
                       END-IF
                   ELSE
      *                院外判定
                       IF      ACCT-ZAITEN         =   ZERO
                           MOVE    1               TO  FLG-INGAI
                       END-IF
                   END-IF
                   IF     (FLG-ZAITAKU         =   1    )  OR
                          (FLG-INNAI           =   1    )  OR
                          (FLG-INGAI           =   1    )
                       PERFORM VARYING     IDXA    FROM    1   BY  1
                               UNTIL      (IDXA    >   5   )  OR
                                          (SRY-SRYCD (IDXA)   =   SPACE)
                         IF      SRY-SRYKBN        =   "14"
                           IF      SRY-SRYCD (IDXA)(1:1)   NOT =   "1"
                               MOVE    SRY-SRYCD (IDXA)    TO  WRK-SRYCD
                               PERFORM 3103-SRYACT-HEN-SEC
                           END-IF
                         ELSE
                           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"  OR
                                                               "0"  OR
                                                               "8") AND
                                  (SRY-SRYCD (IDXA)    NOT =
                                                           "630010002")
      *                          用量コード対象外
                              AND (SRY-SRYCD (IDXA)(1:7)
                                                   NOT =   "0992000")
                               MOVE    SRY-SRYCD (IDXA)    TO  WRK-SRYCD
                               MOVE    ZERO                TO  FLG-NAME
                               PERFORM 3103-SRYACT-HEN-SEC
                           END-IF
                         END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
      *        処方箋料がある時、印刷をおこなう
               IF      SRY-SRYSYUKBN       =   "820"
                   MOVE    1                   TO  FLG-SYOHOU
               END-IF
               IF  SRY-SRYKBN   =  "98"
                 PERFORM  20030-SRYKBN-98-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HENSYU          =   1
               PERFORM 31031-NAIYOU-HENSYU-SEC
               INITIALIZE                  WRK-MEISAI-AREA
               MOVE    ZERO                TO  FLG-HENSYU
           END-IF
      *
      *    剤ごとに、編集する
           IF      IDX-S               =   ZERO
               CONTINUE
           ELSE
               PERFORM 20029-SRYACT-HENSYU-SEC
           END-IF
      *
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *        コメント(備考)(院内.院外共通)編集処理
      *****************************************************************
       20030-SRYKBN-98-HEN-SEC         SECTION.
      *
      *    診療会計より、点数チェック
           PERFORM 20021-SRYACCT-SYORI-SEC
      *
           IF      WRK-KAISU           >   ZERO
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   5   )  OR
                                  (SRY-SRYCD (IDXA)   =   SPACE)
      *        後発医薬品への変更
               IF      SRY-SRYCD (IDXA)    =   "099209902"
                                           OR  "099209904"
                   IF  SRY-SRYCD (IDXA)   =   "099209902"
                       MOVE     1     TO    FLG-KOUHATU-HEN
                   ELSE
                       MOVE     1     TO    FLG-KOUHATUFUKA-IN
                   END-IF
               ELSE
                   MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            用法
                   IF   SRY-SRYCD (IDXA)(1:3)   =   "001"
                     PERFORM  310300-YOUHO-NAME-SEC
                   ELSE
      *                その他
                       IF      SRY-INPUTNUM (IDXA)     =   ZERO
                           MOVE    TNS-NAME        TO  WRK-TNS-NAME
                       ELSE
                           PERFORM 31032-PTCOM-SET-SEC
                       END-IF
                   END-IF
                   ADD     1                   TO  IDX-BIKOU
                   IF      IDX-BIKOU           <=  9
                       MOVE    WRK-TNS-NAME        TO  WRK-BIKOU-NAME
                                                           (IDX-BIKOU)
                   END-IF
               END-IF
      *
               END-PERFORM
           END-IF
           .
       20030-SRYKBN-98-HEN-EXT.
           EXIT.
      *****************************************************************
      *    剤毎の編集処理
      *****************************************************************
       20029-SRYACT-HENSYU-SEC         SECTION.
      *
      *    最後の行に回数を編集
           IF      WRK-SRYCD (1:1)     =   "6"
               ADD     1                   TO  IDX-S
           END-IF
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF     (WRK-SRYKBN          =   "21"  )  AND
                  (FLG-SOURYOHEN       =   1     )
               MOVE    "23"                TO  WRK-SRYKBN
           END-IF
      *
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
      *
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
           IF      ORCHC19-KBN         =   "1"  OR  "B"
                                       OR  "C"  OR  "D"
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
               WHEN    "23"
               WHEN    "14"
                   IF      WRK-NAIYOU-Z (IDX-S)  =   SPACE
                       COMPUTE IDX-S             =   IDX-S    -   1
                   END-IF
           END-EVALUATE
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
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
           IF   ACCT-ZAIKBN  =  1
             MOVE    1                   TO   FLG-TAIGAI
           ELSE
             MOVE    JYURRK-SRYYMD(7:2)  TO   IDX-ACT
             MOVE    ACCT-DAY (IDX-REN IDX-ACT)   TO   WRK-KAISU
           END-IF
      *
           .
       20021-SRYACCT-SYORI-EXT.
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
           MOVE    19                  TO  IDX-MAX
      *
           IF     (IDX-S   +   IDX)    >   IDX-MAX
               MOVE    "次ページへ続く"    TO  WRK-NAIYOU
                                                 (WRK-PAGE-MAX 20)(50:)
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
      *    処方内容編集処理
      *****************************************************************
       3103-SRYACT-HEN-SEC         SECTION.
      *
           IF      SRY-SRYCD (IDXA)    =   "099209903"
                               OR  "099209905"  OR  "099209906"
      *        後発医薬品へ変更不可
               PERFORM 31039-KOUHATU-NAME-SEC
           ELSE
               IF      FLG-HENSYU          =   1
                   PERFORM 31031-NAIYOU-HENSYU-SEC
                   INITIALIZE                  WRK-MEISAI-AREA
                   MOVE    ZERO                TO  FLG-HENSYU
               END-IF
      *        コード毎の編集
               PERFORM 31032-TENSU-HENSYU-SEC
               MOVE    1                   TO  FLG-HENSYU
           END-IF
           MOVE    SRY-SRYCD(IDXA)      TO   WRK-KOU-SRYCD
      *
           .
       3103-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    後発医薬品へ変更不可編集処理
      *****************************************************************
       31039-KOUHATU-NAME-SEC         SECTION.
      *
           IF  WRK-KOU-SRYCD  =  "099209905"  OR  "099209906"
             COMPUTE    WRK-KOU-KETA2  =  WRK-KOU-KETA  +  1
           ELSE
             MOVE    1                TO   WRK-KOU-KETA2  
           END-IF
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               STRING  "【"            DELIMITED  BY  SIZE
                       TNS-NAME        DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
                                    INTO    WRK-KOU-NAME(WRK-KOU-KETA2:)
               END-STRING
               IF  WRK-KOU-SRYCD  =  "099209905"  OR  "099209906"
                 COMPUTE  WRK-KOU-KETA  =  (TNS-YUKOKETA  *  2)  +  4
                                        +  WRK-KOU-KETA
                                            
               ELSE
                 COMPUTE  WRK-KOU-KETA  =  (TNS-YUKOKETA  *  2)  +  4
               END-IF
      *
           END-IF
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-1
      *
           MOVE    1               TO  FLG-KOUHATUFUKA-ARI
           .
       31039-KOUHATU-NAME-EXT.
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
           IF  (WRK-M-SRYCD(1:1)  =  "6"  OR  "7" )
               OR  (WRK-M-SRYCD(1:3)  =  "059" )
      *        数量
             IF   WRK-SURYO-N22  =  SPACE
      *    １８文字まで
               IF   WRK-SURYOU (IDX-S)  NOT =  SPACE
                 ADD     1               TO   IDX-S
                 MOVE    ALL "　"        TO   WRK-NO(IDX-S)
                 MOVE    ALL "　"        TO   WRK-K2(IDX-S)
               END-IF
               MOVE    WRK-SURYO-N       TO   WRK-SURYOU(IDX-S)
               MOVE    WRK-M-TANI        TO   WRK-TANI(IDX-S)
             ELSE
      *   ２２文字まで
               IF   WRK-SURYOU4(IDX-S)  NOT =  SPACE
                 ADD    1                TO   IDX-S
                 MOVE    ALL "　"        TO   WRK-NO(IDX-S)
                 MOVE    ALL "　"        TO   WRK-K2(IDX-S)
               END-IF
               MOVE    WRK-SURYO-N22     TO   WRK-SURYOU4(IDX-S)
               MOVE    WRK-M-TANI        TO   WRK-TANI4(IDX-S)
             END-IF
           END-IF
      *
           INSPECT WRK-MEISYOU1 (IDX-S)  REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-SURYOU (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-TANI   (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    後発医薬品変更可能判定
           IF      FLG-KOUHATU         =   1
               MOVE    1               TO  FLG-KOUHATU-CHK
           END-IF
           IF      FLG-KOUHATU-1       =   1
               MOVE    1               TO  FLG-KOUHATU-1-CHK
           END-IF
           IF (FLG-KOUHATU    =    1 ) OR
              (FLG-KOUHATU-1  =    1 )
              MOVE    1    TO    FLG-KOUHATU-ARI
           END-IF
           MOVE    ZERO                TO  FLG-KOUHATU-1
           MOVE    ZERO                TO  FLG-KOUHATU
      *
           .
       31031-NAIYOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    コード毎の編集処理
      *****************************************************************
       31032-TENSU-HENSYU-SEC         SECTION.
      *
      *    点数検索
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    内容
           IF      SRY-SRYCD (IDXA)(1:1)   =   "6"
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
      *        点数マスタ更新前
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
               IF   SRY-SRYCD (IDXA)(1:3)   =   "001"
                 PERFORM  310300-YOUHO-NAME-SEC
               ELSE
      *            その他
                   IF      FLG-NAME            =   ZERO
                       IF      SRY-INPUTNUM (IDXA) =   ZERO
                           MOVE    TNS-NAME        TO  WRK-TNS-NAME
                       ELSE
                           PERFORM 31032-PTCOM-SET-SEC
                       END-IF
                   ELSE
                       MOVE    WRK-COM-NAME        TO  WRK-TNS-NAME
                   END-IF
               END-IF
           END-IF
           MOVE    SRY-SRYCD (IDXA)    TO  WRK-M-SRYCD
           MOVE    WRK-TNS-NAME        TO  WRK-M-NAME
      *
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"  OR  "7" )  OR
                  (SRY-SRYCD (IDXA)(1:3)   =   "059"        )
      *        数量
               MOVE    SRY-SRYSURYO (IDXA)     TO  WRK-SURYO
               IF      WRK-SURYO           =   ZERO
                   MOVE    SPACE               TO  WRK-SURYO-N
                   MOVE    SPACE               TO  WRK-SURYO-N22
               ELSE
                   PERFORM 10011-SURYO-HEN-SEC
               END-IF
               MOVE    WRK-SURYO-N             TO  WRK-M-SURYOU
      *        数量単位
               MOVE    TNS-TANINAME            TO  WRK-M-TANI
           END-IF
      *
      *    免除の判定（在宅薬剤で注射以外のもの）
           IF   FLG-ZAITAKU  =  1
             IF  ( TNS-SRYCD(1:1)  =  "6" )
                 AND  (TNS-YKZKBN  NOT =  4 )
               MOVE    1         TO  WRK-MENJYO-FLG
             END-IF
           END-IF
      *
           .
       31032-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法名称編集処理
      *****************************************************************
       310300-YOUHO-NAME-SEC         SECTION.
      *
           IF  ( SRY-INPUTNUM(IDXA)  =  ZERO )
               OR   ( WRK-COM-NAME  NOT =  SPACE )
             IF  WRK-COM-NAME NOT =  SPACE
               MOVE    WRK-COM-NAME      TO  WRK-YOH-NAME
             ELSE
               MOVE    TNS-NAME          TO  WRK-YOH-NAME
             END-IF
             MOVE    SPACE               TO  WRK-TNS-NAME
             STRING     "【"             DELIMITED   BY  SIZE
                        WRK-YOH-NAME     DELIMITED   BY  "    "
                        "】"             DELIMITED   BY  SIZE
                                         INTO        WRK-TNS-NAME
             END-STRING
           ELSE
      *    患者コメントより
             MOVE    SPACE               TO  WRK-TNS-NAME
             PERFORM  31032-PTCOM-SET-SEC
             IF   WRK-TNS-NAME  =  SPACE
               MOVE    TNS-NAME          TO  WRK-TNS-NAME
             END-IF
             MOVE    WRK-TNS-NAME        TO  WRK-YOH-NAME
             MOVE    SPACE               TO  WRK-TNS-NAME
             STRING     "【"             DELIMITED   BY  SIZE
                        WRK-YOH-NAME     DELIMITED   BY  "    "
                        "】"             DELIMITED   BY  SIZE
                                         INTO        WRK-TNS-NAME
             END-STRING
           END-IF
           .
       310300-YOUHO-NAME-EXT.
           EXIT.
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
      *
      *****************************************************************
      *     名称編集処理
      *****************************************************************
       31031-MEISYOU-HEN-SEC           SECTION.
      *
      *    桁数
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-M-NAME          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-M-KETA
      *    薬剤・器材編集
           IF     (WRK-M-SRYCD(1:1)    =   "6"  OR  "7" )  OR
                  (WRK-M-SRYCD(1:3)    =   "059"  )
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
           MOVE    SPACE               TO  WRK-TNS-NAME
           IF      (WRK-M-KETA +   WRK-KOU-KETA)  <=  80
               STRING   WRK-M-NAME     DELIMITED  BY  SPACE
                        WRK-KOU-NAME   DELIMITED  BY  SPACE
                                       INTO  WRK-TNS-NAME
               END-STRING
               MOVE    WRK-TNS-NAME    TO  WRK-MEISYOU3 (IDX-S)
           ELSE
               MOVE    WRK-M-NAME      TO  WRK-MEISYOU3 (IDX-S)
               IF      WRK-M-KETA      <=  80
                   ADD     1               TO  IDX-S
                   MOVE    ALL "　"        TO  WRK-NO (IDX-S)
                   MOVE    ALL "　"        TO  WRK-K2 (IDX-S)
                   MOVE    WRK-KOU-NAME    TO  WRK-MEISYOU3 (IDX-S)
               ELSE
                   MOVE    WRK-M-NAME(81:) TO  WRK-NAME
                   STRING  WRK-NAME        DELIMITED  BY  SPACE
                           WRK-KOU-NAME    DELIMITED  BY  SPACE
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
      *
      *****************************************************************
      *     数量単位編集処理
      *****************************************************************
       31031-TANI-HENSYU-SEC            SECTION.
      *
           MOVE    TNS-TANINAME(1:2)     TO  WRK-TANINAME
      *
           EVALUATE    TNS-TANICD
      *            セット
               WHEN    009
                   MOVE    "Set"               TO  WRK-TANINAME
      *            トローチ
               WHEN    013
                   MOVE    TNS-TANINAME(1:2) TO  WRK-TANINAME
      *            アンプル
               WHEN    014
                   MOVE    "A"                 TO  WRK-TANINAME
      *            カプセル
               WHEN    015
                   MOVE    "Cap"               TO  WRK-TANINAME
      *            バイアル
               WHEN    039
                   MOVE    "B"                 TO  WRK-TANINAME
      *            キット
               WHEN    051
                   MOVE    "Kit"               TO  WRK-TANINAME
      *            フィート
               WHEN    030
                   MOVE    "FT"                TO  WRK-TANINAME
      *            ｍｇ
               WHEN    032
                   MOVE    "mg"                TO  WRK-TANINAME
               WHEN    034
                   MOVE    "Kg"                TO  WRK-TANINAME
               WHEN    035
                   MOVE    "cc"                TO  WRK-TANINAME
               WHEN    036
                   MOVE    "mL"                TO  WRK-TANINAME
               WHEN    038
                   MOVE    "mLV"                TO  WRK-TANINAME
               WHEN    040
                   MOVE    "cm"                TO  WRK-TANINAME
               WHEN    041
                   MOVE    "cm2"               TO  WRK-TANINAME
               WHEN    042
                   MOVE    "m"                 TO  WRK-TANINAME
               WHEN    043
                   MOVE    "uCi"               TO  WRK-TANINAME
               WHEN    044
                   MOVE    "mCi"               TO  WRK-TANINAME
               WHEN    045
                   MOVE    "ug"                TO  WRK-TANINAME
               WHEN    048
                   MOVE    "GBq"                TO  WRK-TANINAME
               WHEN    049
                   MOVE    "MBq"                TO  WRK-TANINAME
               WHEN    050
                   MOVE    "KBq"                TO  WRK-TANINAME
               WHEN    058
                   MOVE    "mLg"                TO  WRK-TANINAME
           END-EVALUATE
           .
       41011231-TANI-HENSYU-EXT.
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
      *
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
      *    小数点３桁まで数量表示用編集処理
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
           IF   WRK-SURYO2-X21  =  SPACE
      *    整数３桁・小数部５桁まで
             MOVE    WRK-SURYO2-X29      TO   WRK-MOJI
             MOVE    9                   TO   WRK-MOJISU
             PERFORM  2009-HENKAN-SEC
             MOVE    WRK-ZENKAKU-G(1:18) TO   WRK-SURYO-N
           ELSE
      *    整数３桁以上・小数部５桁
             MOVE    WRK-SURYO-X2        TO   WRK-MOJI
             MOVE    11                  TO   WRK-MOJISU
             PERFORM  2009-HENKAN-SEC
             MOVE    WRK-ZENKAKU-G(1:22) TO   WRK-SURYO-N22
           END-IF
           .
       100112-SURYO-HEN05-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       31032-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
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
      *
      *****************************************************************
      *    中途終了時の発行処理  20040929  Ver 2.4.0移植  MOTO
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
      *--- 20110107 CHANGE START ---------------------------------------
      *        投薬と在宅のみ対象とする
      *         IF      WKSRY-SRYKBN        =   "21"
      *                                     OR  "22"
      *                                     OR  "23"
      *                                     OR  "14"
      *                                     OR  "98"
               IF   (WKSRY-SRYKBN    =   "21"  OR  "22"  OR
                                         "23"  OR  "14"  OR  "98" )
      *--- 20110107 CHANGE END   ---------------------------------------
      *           包括保険分は対象外
                  AND (WRK-HKNCOMBINUM NOT =   "9999" )
      *                薬評分対象外
                  AND  ( WKSRY-ZAIKBN  NOT =  1 )
      *
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
      *        処方編集初期クリア処理
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
                   PERFORM 31031-NAIYOU-HENSYU-SEC
                   INITIALIZE                  WRK-MEISAI-AREA
                   MOVE    ZERO                TO  FLG-HENSYU
               END-IF
      *        剤終了時、編集へ
               IF      IDX-S           >   ZERO
                   PERFORM 20032-PRINT-HENSYU-SEC
               END-IF
               IF      IDX             >   ZERO
      *            印刷
                   PERFORM 200212-PRINT-CHK-SEC
               END-IF
      *
           END-PERFORM
           .
       2003-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方編集初期クリア処理
      *****************************************************************
       20031-WKSYOHOU-INIT-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    1                   TO  WRK-PAGE-MAX
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  FLG-MAYAKU
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  FLG-OK2
           MOVE    ZERO                TO  WRK-YKZTEN
           MOVE    ZERO                TO  WRK-KIZAITEN
           INITIALIZE                WRK-BIKOU-AREA
           MOVE    ZERO                TO  FLG-KOUHATU-HEN
           MOVE    ZERO                TO  FLG-KOUHATU-CHK
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-1
           MOVE    ZERO                TO  FLG-KOUHATU-1-CHK
           INITIALIZE                  WRK-MEISAI-AREA
           MOVE    ZERO                TO  FLG-HENSYU
           MOVE    ZERO                TO  FLG-SOURYOHEN
           .
       20031-WKSYOHOU-INIT-EXT.
           EXIT.
      *****************************************************************
      *    処方編集初期クリア処理
      *****************************************************************
       20032-WKSRYACT-CHK-SEC         SECTION.
      *
           IF      WKSRY-RENNUM        =   1
               IF      FLG-HENSYU          =   1
                   PERFORM 31031-NAIYOU-HENSYU-SEC
                   INITIALIZE                  WRK-MEISAI-AREA
                   MOVE    ZERO                TO  FLG-HENSYU
               END-IF
      *        剤終了時、編集へ
               IF      IDX-S           >   ZERO
                   PERFORM 20032-PRINT-HENSYU-SEC
               END-IF
      *        回数
               MOVE    WKSRY-ZAIKAIKEI     TO  WRK-KAISU
               MOVE    WKSRY-SRYSYUKBN     TO  WRK-SRYSYUKBN
               MOVE    WKSRY-SRYKBN        TO  WRK-SRYKBN
               MOVE    ZERO                TO  FLG-OK
               MOVE    ZERO                TO  FLG-OK2
               MOVE    ZERO                TO  IDX-S
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
      *    コメント（備考）（院内・院外共通）
           IF      WKSRY-SRYKBN        =   "98"
               PERFORM 20033-WKSRYKBN-98-HEN-SEC
           ELSE
               PERFORM 20031-WKSRYACT-HEN-SEC
           END-IF
      *
           .
       20032-WKSRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為内容を診療行為へ
      *****************************************************************
       20031-WKSRYACT-HEN-SEC         SECTION.
      *
           INITIALIZE                  SRYACT-REC
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )  OR
                              (WKSRY-SRYCD (IDXA) =   SPACE)
               IF    ((WKSRY-SRYCD (IDXA)(1:1) =   "6"  OR
                                                   "7"  OR
                                                   "0"  OR
                                                   "8") OR
                      (WKSRY-SRYCD (IDXA)(1:3) =   "059"))   AND
                      (WKSRY-SRYCD (IDXA)  NOT =  "630010002")
      *             用量コード対象外
                  AND (WKSRY-SRYCD (IDXA)(1:7)  NOT =  "0992000")
                   MOVE    WKSRY-SRYCD (IDXA)  TO  SRY-SRYCD (IDXA)
      *            数量
                   MOVE    WKSRY-SRYSURYO (IDXA)
                                               TO  SRY-SRYSURYO (IDXA)
      *            回数
                   MOVE    WKSRY-SRYKAISU (IDXA)
                                               TO  SRY-SRYKAISU (IDXA)
                   MOVE    WKSRY-SRYCD (IDXA)  TO  WRK-SRYCD
      *
                   MOVE    SPACE               TO  WRK-COM-NAME
                   MOVE    ZERO                TO  FLG-NAME
                   IF    ((WKSRY-SRYCD (IDXA)(1:1) =    "8")   OR
                          (WKSRY-SRYCD (IDXA)(1:3) =    "008")) AND
                          (WKSRY-INPUTCOMENT (IDXA)
                                               NOT =   SPACE)
                       MOVE    WKSRY-INPUTCOMENT (IDXA)
                                               TO  WRK-COM-NAME
                       MOVE    1               TO  FLG-NAME
                   END-IF
      *            内容編集
                   PERFORM 3103-SRYACT-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       20031-WKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    中途終了剤毎の編集処理
      *****************************************************************
       20033-WKSRYKBN-98-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )  OR
                              (WKSRY-SRYCD (IDXA) =   SPACE)
      *        後発医薬品への変更
               IF      WKSRY-SRYCD (IDXA)      =   "099209902"
                                               OR  "099209904"
                 IF  ORCHC19-SRYCD (IDX-L IDX-L2)   =   "099209902"
                   MOVE    1                   TO  FLG-KOUHATU-HEN
                 ELSE
                   MOVE    1                   TO  FLG-KOUHATUFUKA-IN
                 END-IF
               ELSE
      *
               MOVE    WKSRY-SRYCD (IDXA)  TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF    ((WKSRY-SRYCD (IDXA)(1:1) =    "8"  ) OR
                      (WKSRY-SRYCD (IDXA)(1:3) =    "008")) AND
                      (WKSRY-INPUTCOMENT (IDXA)
                                           NOT =   SPACE)
                   MOVE    WKSRY-INPUTCOMENT (IDXA)
                                           TO  WRK-TNS-NAME
               ELSE
                   MOVE    TNS-NAME        TO  WRK-TNS-NAME
               END-IF
      *+         IF   WKSRY-SRYCD(IDXA)(1:3)  =  "001"
      *+           MOVE    SPACE           TO  WRK-TNS-NAME
      *+           STRING  "【"            DELIMITED   BY  SIZE
      *+                   TNS-NAME        DELIMITED   BY  "    "
      *+                   "】"            DELIMITED   BY  SIZE
      *+              INTO                 WRK-TNS-NAME
      *+           END-STRING
      *+
      *    用法コメント名称
               IF  (WKSRY-SRYCD(IDXA)(1:3)  =  "001")
                   AND  (WKSRY-INPUTCOMENT(IDXA)  NOT =  SPACE)
                 MOVE    WKSRY-INPUTCOMENT (IDXA)  TO  WRK-TNS-NAME
               END-IF
      *+
               ADD     1                   TO  IDX-BIKOU
               IF      IDX-BIKOU           <=  9
                   MOVE    WRK-TNS-NAME        TO  WRK-BIKOU-NAME
                                                           (IDX-BIKOU)
               END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       20033-WKSRYKBN-98-HEN-EXT.
           EXIT.
      *****************************************************************
      *    中途終了剤毎の編集処理
      *****************************************************************
       20032-PRINT-HENSYU-SEC         SECTION.
      *
      *    投薬で、点数がゼロのもを対象（院外）
           IF      ORCHC19-KBN         =   "4" 
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI        =   ZERO   )
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
      *
           IF      FLG-OK2             =   ZERO
               IF      IDX-Z               >   ZERO
                   COMPUTE IDX-Z           =   IDX-Z     -   1
               END-IF
           ELSE
               PERFORM 200321-HENSYU-SYORI-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  WRK-YKZTEN
           MOVE    ZERO                TO  WRK-KIZAITEN
           MOVE    ZERO                TO  FLG-SOURYOHEN
      *
           .
       20032-PRINT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    中途終了剤毎の編集処理
      *****************************************************************
       200321-HENSYU-SYORI-SEC         SECTION.
      *
      *    最後の行に回数を編集
           IF      WRK-SRYCD (1:1)     =   "6"
               ADD     1                   TO  IDX-S
           END-IF
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF     (WRK-SRYKBN          =   "21"  )  AND
                  (FLG-SOURYOHEN       =   1     )
               MOVE    "23"                TO  WRK-SRYKBN
           END-IF
      *    最後の行に回数を編集
           EVALUATE    WRK-SRYKBN
               WHEN    "21"
               WHEN    "22"
                   PERFORM 20022-KAISU-HEN-SEC
           END-EVALUATE
      *
           EVALUATE    WRK-SRYKBN
               WHEN    "21"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "日分）"              TO  WRK-HINICHI (IDX-S)
               WHEN    "22"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "回分）"              TO  WRK-HINICHI (IDX-S)
               WHEN    "23"
               WHEN    "14"
                   IF      WRK-NAIYOU-Z (IDX-S)  =   SPACE
                       COMPUTE IDX-S             =   IDX-S    -   1
                   END-IF
           END-EVALUATE
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    帳票用編集処理
           PERFORM 20022-PRINT-HEN-SEC
      *
           .
       200321-HENSYU-SYORI-EXT.
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
               MOVE    ORCHC19-ZAIKAIKEI(IDX-L)   TO  WRK-KAISU
               MOVE    ORCHC19-SRYSYUKBN(IDX-L)   TO  WRK-SRYSYUKBN
               MOVE    ORCHC19-SRYKBN   (IDX-L)   TO  WRK-SRYKBN
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    ZERO                TO  FLG-HENSYU
               MOVE    ZERO                TO  IDX-S
               MOVE    ZERO                TO  IDXA
               INITIALIZE                  SRYACT-REC
               PERFORM VARYING     IDX-L2  FROM    1   BY  1
                       UNTIL      (IDX-L2  >   50  )   OR
                                  (ORCHC19-SRYCD (IDX-L IDX-L2)
                                               =   SPACE )
      *            コメント（備考）（院内・院外共通）
                   IF      WRK-SRYKBN          =   "98"
                       PERFORM 20042-ORCHC19-98-HEN-SEC
                   ELSE
      *                用量コード対象外
                       IF      ORCHC19-SRYCD (IDX-L IDX-L2)(1:7)
                                                   =   "0992000"
                           CONTINUE
                       ELSE
                           PERFORM 20041-ORCHC19-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      FLG-HENSYU          =   1
                   PERFORM 31031-NAIYOU-HENSYU-SEC
                   INITIALIZE                  WRK-MEISAI-AREA
                   MOVE    ZERO                TO  FLG-HENSYU
               END-IF
      *        剤終了時、編集へ
               IF      IDX-S           >   ZERO
                   PERFORM 20043-END-HENSYU-SEC
               END-IF
           END-PERFORM
      *    印刷
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
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    1                   TO  WRK-PAGE-MAX
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  FLG-MAYAKU
      *
           INITIALIZE                  WRK-BIKOU-AREA
           MOVE    ZERO                TO  FLG-KOUHATU-HEN
           MOVE    ZERO                TO  FLG-KOUHATU-CHK
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-1
           MOVE    ZERO                TO  FLG-KOUHATU-1-CHK
           INITIALIZE                  WRK-MEISAI-AREA
           MOVE    ZERO                TO  FLG-HENSYU
           MOVE    ZERO                TO  FLG-SOURYOHEN
      *
           .
       200411-SYOHOU-SPAINIT-EXT.
           EXIT.
      *****************************************************************
      *    前回処方編集処理
      *****************************************************************
       20041-ORCHC19-HEN-SEC         SECTION.
      *
           ADD    1                    TO  IDXA
           IF   IDXA  >  5
             INITIALIZE                SRYACT-REC
             MOVE    1                 TO  IDXA
           END-IF
           MOVE    ORCHC19-SRYCD(IDX-L IDX-L2)  TO  SRY-SRYCD (IDXA)
      *    数量
           MOVE    ORCHC19-SRYSURYO(IDX-L IDX-L2)
                                       TO  SRY-SRYSURYO(IDXA)
      *    回数
           MOVE    ORCHC19-SRYKAISU(IDX-L IDX-L2) TO  SRY-SRYKAISU(IDXA)
           MOVE    ORCHC19-SRYCD(IDX-L IDX-L2)  TO  WRK-SRYCD
      *
           MOVE    SPACE               TO  WRK-COM-NAME
           MOVE    ZERO                TO  FLG-NAME
           IF   ((ORCHC19-SRYCD(IDX-L IDX-L2)(1:1)  =  "8")
               OR  (ORCHC19-SRYCD(IDX-L IDX-L2)(1:3)  =  "008")
               OR  (ORCHC19-SRYCD(IDX-L IDX-L2)(1:3)  =  "001"))
               AND  (ORCHC19-INPUTCOMENT(IDX-L IDX-L2)  NOT =  SPACE)
             MOVE    ORCHC19-INPUTCOMENT(IDX-L IDX-L2)  TO  WRK-COM-NAME
             MOVE    1                 TO   FLG-NAME
           END-IF
      *    内容編集
           PERFORM 3103-SRYACT-HEN-SEC
      *
           .
       20041-ORCHC19-HEN-EXT.
           EXIT.
      *****************************************************************
      *    前回処方（選択なし）処理
      *****************************************************************
       20042-ORCHC19-98-HEN-SEC         SECTION.
      *
      *    後発医薬品への変更
           IF      ORCHC19-SRYCD (IDX-L IDX-L2)    =   "099209902"
                                                   OR  "099209904"
             IF  ORCHC19-SRYCD (IDX-L IDX-L2)   =   "099209902"
               MOVE    1                   TO  FLG-KOUHATU-HEN
             ELSE
               MOVE    1                   TO  FLG-KOUHATUFUKA-IN
             END-IF
           ELSE
      *
           MOVE    ORCHC19-SRYCD (IDX-L IDX-L2)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF    ((ORCHC19-SRYCD (IDX-L IDX-L2)(1:1)
                                       =    "8")   OR
                  (ORCHC19-SRYCD (IDX-L IDX-L2)(1:3)
                                       =    "008")) AND
                  (ORCHC19-INPUTCOMENT (IDX-L IDX-L2)
                                   NOT =   SPACE)
               MOVE    ORCHC19-INPUTCOMENT (IDX-L IDX-L2)
                                           TO  WRK-TNS-NAME
           ELSE
               MOVE    TNS-NAME            TO  WRK-TNS-NAME
           END-IF
           IF      ORCHC19-SRYCD (IDX-L IDX-L2)(1:3) =   "001"
               MOVE    SPACE           TO  WRK-TNS-NAME
               STRING  "【"            DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  "    "
                       "】"            DELIMITED   BY  SIZE
                  INTO                 WRK-TNS-NAME
               END-STRING
           END-IF
           ADD     1                   TO  IDX-BIKOU
           IF      IDX-BIKOU           <=  9
               MOVE    WRK-TNS-NAME        TO  WRK-BIKOU-NAME
                                                           (IDX-BIKOU)
           END-IF
      *
           END-IF
           .
       20042-ORCHC19-98-HEN-EXT.
           EXIT.
      *****************************************************************
      *    剤終了時編集処理
      *****************************************************************
       20043-END-HENSYU-SEC         SECTION.
      *
      *    最後の行に回数を編集
           IF      WRK-SRYCD (1:1)     =   "6"
               ADD     1                   TO  IDX-S
           END-IF
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF     (WRK-SRYKBN          =   "21"  )  AND
                  (FLG-SOURYOHEN       =   1     )
               MOVE    "23"                TO  WRK-SRYKBN
           END-IF
      *
           IF      ORCHC19-KBN         =   "A" OR  "N"
      *        最後の行に回数を編集
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
               WHEN    "23"
               WHEN    "14"
                   IF      WRK-NAIYOU-Z (IDX-S)  =   SPACE
                       COMPUTE IDX-S             =   IDX-S    -   1
                   END-IF
           END-EVALUATE
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    帳票用編集処理
           PERFORM 20022-PRINT-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  WRK-YKZTEN
           MOVE    ZERO                TO  WRK-KIZAITEN
           MOVE    ZERO                TO  FLG-SOURYOHEN
      *
           .
       20043-END-HENSYU-EXT.
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
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(9)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-SYS1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-SYS1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
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
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
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
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
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
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
      *
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
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
      *
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
      *    診療科歴データ順読み
      *****************************************************************
       975-SRYKARRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC 
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
