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
      *PROGRAM-ID.             ORCHC62.
       PROGRAM-ID.             A00000RA01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳印刷
      *  コンポーネント名  : お薬手帳印刷（ＨＣ６２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/03/05    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.06.00    NACL-多々納  11/02/22  用法コードコメント対応
      *  04.06.00    NACL-森脇    11/03/10  用量割合コード対応
      *  04.07.00    NACL-多々納  12/09/03  ＱＲコード対応
      *                                     商品名コード対応
      *  04.08.00    NACL-多々納  13/07/XX  電子版お薬手帳アプリ連携対応他
      *  04.08.00    NACL-多々納  14/07/30  一時ディレクトリ対応
      *  04.08.01    NACL-森脇    15/08/12  処方せん備考欄コメント表示
      *  04.08.02    NACL-森脇    15/09/14  注意事項・効能効果表示
      *****************************************************************
      *  作成日付    作業者    記述
      *  15/11/18    萩野      新規作成
      *  16/01/26    萩野      効能効果の画像グループ連番に「0」番と印字される不具合を修正
      *                        「処方日」を「診療日」に文言を変更
      *                        「●画像」を「●画像診断」に文言を変更
      *  16/01/27    萩野      効能効果・注意事項のみを印刷できるように修正
      *  16/02/08    萩野      「●内服、●頓服、●外用」にグループを分け、
      *                        連番が[1)]から振り直すように修正
      *                        在宅を印字処理を修正
      *                        「効能効果・注意事項」の薬剤名の後ろに数量を
      *                        印字されるように修正
      *                        「効能効果・注意事項」の薬剤名の前に用法を
      *                        印字されるように修正
      *  16/02/16    萩野      用法の先頭に空白を追加して印字するよう修正
      *  16/03/02    萩野      プログラムオプションに用法コメント【YOUCOM】を取得するように修正
      *                        お薬手帳(ORCHC62)で印字されるコメント文と、
      *                        プログラムオプション【YOUCOM】で設定したコードと
      *                        一致するコードのコメント文を印字するように修正
      *                        また「注射、検査、画像診断」については
      *                        プログラムオプション【YOUCOM】で設定したコードと
      *                        一致するコードのコメント文のみを印字するようにする
      *  16/03/07    萩野      罫線のアンダーバーを全角に修正
      *  16/04/06    萩野      請求確認画面から実行時の15秒計算を修正
      *                        複数科受診時に保険医名が違う薬剤内容が同じ帳票が
      *                        ２枚印刷される不具合を修正
      *  16/04/27    萩野      プログラムオプションの効能効果・注意事項の
      *                        印字パターンを薬剤の帳票と効能効果のどちらかを
      *                        発行するように変更。
      *  16/04/28    萩野      複数科受診時の帳票の分け方を本体ＰＧと
      *                        同様になるよう修正
      *  16/05/09    萩野      効能効果・注意事項について、検査と画像診断の
      *                        場合は検査名、診断名も印字されるよう修正。
      *  16/09/26    萩野      院内分、院外分の印字をプログラムオプションで
      *                        選択できるようにする
      *  20/02/20    萩野      ユーザー単位対応
      *  20/03/23    萩野      コメント桁数変更
      *****************************************************************
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
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(50000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    帳票領域
      *--- 2015/11/18 START ------------------------------------------
      *     COPY    "HCM62.INC".
           COPY    "A00000RA01.INC".
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    ＱＲ帳票領域
           COPY    "HCM62Q.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TNSP            PIC 9(01).
      *----(04.08.02)--ADD-START---
           03  FLG-YAKUJYO         PIC 9(01).
      *----(04.08.02)--ADD-END-----
      *
           03  FLG-KBN             PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-SOURYO          PIC 9(01).
      *
           03  FLG-QROK            PIC 9(01).
      *
      *--- 2015/11/18 START ------------------------------------------
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-DRCD            PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
      *
           03  FLG-GROUP-PRT       PIC 9(01).
           03  FLG-GROUP           PIC 9(01).
      *
           03  FLG-SRYKBN2         PIC 9(01).
           03  FLG-SRYKBN3         PIC 9(01).
           03  FLG-SRYKBN6         PIC 9(01).
           03  FLG-SRYKBN7         PIC 9(01).
      *
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-ZAI             PIC 9(03).
      *--- 2015/11/18 START ------------------------------------------
           03  CNT-ZAI2            PIC 9(03).
      *--- 2015/11/18 END   ------------------------------------------
           03  CNT-SRY             PIC 9(02).
           03  CNT-KAISU           PIC 9(01).
      *--- 2015/11/18 START ------------------------------------------
           03  CNT-LINE-MAX        PIC 9(02).
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(03).
           03  IDX-SRY             PIC 9(02).
           03  IDX-KBN             PIC 9(01).
      *H24.9
           03  IDX-QR1             PIC 9(04).
           03  IDX-QR2             PIC 9(04).
           03  IDX-QRB             PIC 9(04).
           03  IDX-QY              PIC 9(04).
           03  IDQ                 PIC 9(04).
      *--- 2015/11/18 START ------------------------------------------
           03  IDX-ST              PIC 9(01).
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-HENSURYO        PIC X(11).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-HEN-AREA.
               05  WRK-HENLINE     PIC X(54).
               05  WRK-HENNAIYO    PIC X(26).
               05  WRK-YKZNAME     PIC X(140).
               05  WRK-YOUHOU      PIC X(140).
               05  WRK-SURYOU      PIC X(11).
               05  WRK-TANI        PIC X(12).
               05  WRK-KAISUU      PIC X(03).
      *
      *----(04.08.02)--UPD-START---
           03  WRK-HENSYUDATA      PIC X(4000).
      *----(04.08.02)--UPD-END-----
      *
           03  WRK-SSTKIJUNCD      PIC X(05).
           03  WRK-HENKBN-G.
               05  WRK-HENKBN      PIC 9(08)V9(05)
                                               OCCURS  5.
      *
           03  WRK-YOURYO          PIC 9(05)V9(05).
           03  WRK-KAISU           PIC 9(01).
      *
      *H24.9
      *    ＱＲ編集
           03  WRK-TNS-NAME        PIC X(140).
           03  WRK-YOH-NAME-G.
               05  WRK-YOH-NAME        OCCURS   5.
                   07  WRK-YOH-NAME-1  PIC X(02).
                   07  WRK-YOH-NAME-2  PIC X(138).
           03  WRK-FILE-CNT        PIC 9(02).
           03  WRK-SYS-TIME        PIC 9(08).
      *    ファイル名
           03  WRK-CSV-FILE        PIC X(256).
           03  WRK-DIRECTORY       PIC X(256).
           03  WRK-DIRECTORY-E     PIC X(256).
           03  WRK-DRCD-E          PIC X(05).
      *ver.4.7
           03  WRK-SHO-NAME        PIC X(140).
           03  WRK-SHO-TANINAME    PIC X(20).
      *
           03  WRK-QRKBN           PIC X(01).
      *
      *--- 2015/11/18 START ------------------------------------------
           03  WRK-MSG                 PIC X(500).
      *
      *    お薬手帳印刷用パラメタ
      *--- 2016/04/28 START ------------------------------------------
      *    ドクター
           03  WRK-KEY-DRCD            PIC X(05).
      *    処理回数
           03  WRK-KEY-CNT             PIC 9(03).
      *    伝票番号（主の伝票番号）
           03  WRK-KEY-DENPNUM         PIC 9(07).
      *--- 2016/04/28 END   ------------------------------------------
      *
           03  WRK-DRCD            PIC X(05).
           03  WRK-DENPNUM         PIC 9(07).
      *
           03  WRK-CHARFLG         PIC 9(01).
           03  WRK-CHARCNT         PIC 9(02).
      *--- 2015/11/18 END   ------------------------------------------
      *--- 2016/02/08 START ------------------------------------------
           03  WRK-GROUPNAME       PIC X(20).
           03  WRK-GROUPNAMEFLG    PIC 9(01).
      *--- 2016/02/08 END   ------------------------------------------
      *
      *--- 2015/11/18 START ------------------------------------------
      *    医師コード 一時領域
       01  H016-DR-AREA.
           03  H016-DRCD             PIC X(05).
       01  H016-DR2-AREA.
           03  H016-DR-TBL         OCCURS  5.
             05  H016-DRCD1          PIC X(05).
             05  H016-DRCD2          PIC X(05).
      *
      *    【請求確認】
      *    伝票番号 一時領域
       01  H016-AREA.
           03  H016-GRP-TBL        OCCURS  5.
             05  H016-DENPNUM        PIC 9(07).
      *--- 2016/02/08 START ------------------------------------------
             05  H016-INGAISHOHOKBN  PIC  X(01).
      *--- 2016/02/08 END   ------------------------------------------
      *
      *    日付/時間 一時領域
       01  TIME-AREA.
           03  TIME-YMD-HMS.
             05  TIME-YMD            PIC 9(08).
      *--- 2016/04/06 START ------------------------------------------
      *       05  TIME-HMS            PIC 9(08).
      *     03  TIME-SYUNOU-HMS       PIC 9(06).
             05  TIME-HMS.
                 07  TIME-HH         PIC 9(02).
                 07  TIME-MM         PIC 9(02).
                 07  TIME-SS         PIC 9(02).
           03  TIME-SYUNOU-HMS.
               05  TIME-SYU-HH       PIC 9(02).
               05  TIME-SYU-MM       PIC 9(02).
               05  TIME-SYU-SS       PIC 9(02).
           03  TIME-STIME            PIC 9(06).
           03  TIME-SYUNOU-STIME     PIC 9(06).
      *--- 2016/04/06 END   ------------------------------------------
      *
      *    パラメタ領域
       01  WRK-PARA-AREA.
           03  WRK-PARA-LDNAME       PIC X(10).
           03  WRK-PARA-SYORIKBN     PIC 9(01).
           03  WRK-PARA-SYSYMD       PIC X(08).
           03  WRK-PARA-SRYYMD       PIC X(08).
           03  WRK-PARA-PTID         PIC 9(10).
           03  WRK-PARA-PTNUM        PIC X(20).
           03  WRK-PARA-SRYKA        PIC 9(02).
           03  WRK-PARA-DRCD         PIC X(05).
           03  WRK-PARA-HKNCOMBINUM  PIC 9(04).
           03  WRK-PARA-SHORINAME    PIC X(20).
           03  WRK-PARA-PRINTNAME    PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    剤内容領域
       01  WRK-SRY-AREA.
           03  WRK-ZAIKAIKEI       PIC 9(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRY-TBL         OCCURS  50.
               05  WRK-SRYSURYO    PIC 9(05)V9(05).
               05  WRK-SRYKAISU    PIC 9(03).
               05  WRK-INPUTCOMENT PIC X(140).
               05  WRK-YOURYOU-TBL.
                   07  WRK-YOURYOU     PIC 9(05)V9(05)
                                       OCCURS   5.
      *
      *    印刷編集領域
       01  WRK-LINE-AREA.
           03  WRK-LINE-1.
               05  WRK-PTNAME      PIC X(54).
           03  WRK-LINE-2.
      *--- 2016/01/26 START ------------------------------------------
      *         05  FILLER          PIC X(06)   VALUE   "処方日".
               05  FILLER          PIC X(06)   VALUE   "診療日".
      *--- 2016/01/26 END   ------------------------------------------
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-SRYYMDG     PIC X(04).
               05  WRK-SRYYMDY     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "年".
               05  WRK-SRYYMDM     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "月".
               05  WRK-SRYYMDD     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
      *--- 2015/11/18 START ------------------------------------------
      *         05  FILLER          PIC X(08)   VALUE   "保険医名".
      *         05  FILLER          PIC X(01)   VALUE   SPACE.
      *         05  WRK-DRNAME      PIC X(18).
           03  WRK-LINE-DRNAME.
               05  FILLER          PIC X(08)   VALUE   "保険医名".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-DRNAME      PIC X(18).
      *--- 2015/11/18 END   ------------------------------------------
           03  WRK-LINE-3.
               05  WRK-HOSPNAME    PIC X(54).
           03  WRK-LINE-4.
      *--- 2015/11/18 START ------------------------------------------
               05  FILLER          PIC X(08)   VALUE   "ＴＥＬ：".
      *--- 2015/11/18 END   ------------------------------------------
               05  WRK-HOSPTEL     PIC X(15).
           03  WRK-LINE-SEN.
               05  FILLER          PIC X(54)   VALUE
      *--- 2016/03/07 START ------------------------------------------
      *         "______________________________________________________".
               "＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿".
      *--- 2016/03/07 END   ------------------------------------------
           03  WRK-LINE-SEN2.
               05  FILLER          PIC X(54)   VALUE
      *--- 2016/03/07 START ------------------------------------------
      *         "   ___________________________________________________".
               "  ＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿".
      *--- 2016/03/07 END   ------------------------------------------
      *
      *
      *H24.9
      *    ＱＲ位置
       01  QR-FILE-NAME-PARA.
      *****03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  QR-FILE-HOSPNUM     PIC 9(02)   VALUE   ZERO.
           03  QR-FILE-FORM-ID     PIC X(05)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  QR-FILE-TERMID      PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  QR-FILE-SYOHMS      PIC 9(08)   VALUE   ZERO.
           03  FILLER              PIC X(04)   VALUE   ".qr_".
           03  QR-FILE-ID          PIC 9(02)   VALUE   ZERO.
           03  FILLER              PIC X(04)   VALUE   ".png".
      *v4.8
       01  QR-FILE-NAME            PIC X(1024).
      *
       01  KUHAKU-FILE-NAME        PIC X(512).
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER          PIC X(05)   VALUE   "data/".
           03  FILLER          PIC X(15)   VALUE   "transparent.jpg".
      *　ＱＲ名称
       01  CONS-FORM-ID        PIC X(05)   VALUE   "HC62Q".
      *H24.9
      *
      *01  CONS-CSVDIR             PIC X(28)   VALUE
      *                            "/var/lib/jma-receipt/jahistc".
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-COMMENT2           PIC X(01).
      *H24.9
           03  PARA-QRKBN              PIC X(01).
           03  PARA-SHOHIN             PIC X(01).
      *----(04.08.01)--ADD-START---
           03  PARA-BIKOU              PIC X(01).
      *----(04.08.01)--ADD-END-----
      *----(04.08.02)--ADD-START---
           03  PARA-KOUNOU             PIC X(01).
      *----(04.08.02)--ADD-END-----
      ***  03  PARA-CSVKBN             PIC X(01).
      ***  03  PARA-CSVDIR             PIC X(256).
      *--- 2015/11/18 START ------------------------------------------
           03  PARA-TYUSYA             PIC X(01).
           03  PARA-KENSA              PIC X(01).
           03  PARA-GAZOU              PIC X(01).
      *--- 2015/11/18 END   ------------------------------------------
      *--- 2016/03/02 START ------------------------------------------
           03  PARA-YOUCOM             PIC X(09).
      *--- 2016/03/02 END   ------------------------------------------
      *--- 2016/09/26 START ------------------------------------------
           03  PARA-SHOHOU             PIC X(01).
      *--- 2016/09/26 END   ------------------------------------------
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数付加情報マスタ
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *----(04.08.01)--ADD-START---
      *    薬情マスタ
           COPY    "CPYAKUJYO.INC".
      *----(04.08.01)--ADD-END-----
      *
      *
      *--- 2015/11/18 START ------------------------------------------
      *    ワーク診療行為マスタ
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *--- 2015/11/18 END   ------------------------------------------
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *ＱＲ対応
      *    ＱＲデータ作成サブパラメタ
           COPY    "CPORCSQRCSV62.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *    ＣＳＶファイル作成パラメタ
      **** COPY    "CPORCSFILEENCODE.INC".
      *    ディレクトリ作成パラメタ
      **** COPY    "CPORCSMKDIR.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *--- 2015/11/18 START ------------------------------------------
      *    お薬手帳発行サブパラメタ
           COPY    "CPORCSCH62.INC".
      *--- 2015/11/18 END   ------------------------------------------
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHC62.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ORCHC62AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
      *--- 2015/11/18 START ------------------------------------------
      *     PERFORM 200-MAIN-SEC
           PERFORM 201-MAIN-SEC
      *--- 2015/11/18 END   ------------------------------------------
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
      *--- 2015/11/18 START ------------------------------------------
           INITIALIZE                  WRK-PARA-AREA
           INITIALIZE                  H016-AREA
                                       H016-DR-AREA
                                       H016-DR2-AREA
                                       TIME-AREA
      *--- 2015/11/18 END   ------------------------------------------
      *H24.9
           INITIALIZE                  ORCSQRCSV62AREA
           MOVE    ZERO                TO  IDX-QR1
           MOVE    ZERO                TO  IDX-QRB
      *
      *--- 2015/11/18 START ------------------------------------------
      *    印刷対象判定用  システム時間の取得
           ACCEPT  TIME-YMD    FROM    DATE
           ACCEPT  TIME-HMS    FROM    TIME
      *--- 2015/11/18 END   ------------------------------------------
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    職員情報
           PERFORM 900-1010-READ-SEC
      *
      *    プログラムオプション設定
      *--- 2015/11/18 START ------------------------------------------
           PERFORM 100-OPTION-SELECT-SEC
      *--- 2015/11/18 END   ------------------------------------------
      *    
      *****CSVファイル作成
      *    MOVE    "CSVKBN"        TO  WRK-PARA-NAME
      *    PERFORM 1001-OPTION-CALL-SEC
      *    MOVE    WRK-PARA(1:1)   TO  PARA-CSVKBN
      *    CSVファイルディレクトリ
      *    MOVE    "CSVDIR"        TO  WRK-PARA-NAME
      *    PERFORM 1001-OPTION-CALL-SEC
      *    MOVE    ORCSPRGOPTION-OT-PARA(1:)
      *                            TO  PARA-CSVDIR
      *
      *    IF     (PARA-QRKBN      =   "1")  AND
      *           (PARA-CSVKBN     =   "1")
      *        IF      PARA-CSVDIR     =   SPACE
      *            MOVE    CONS-CSVDIR     TO  WRK-DIRECTORY
      *        ELSE
      *            MOVE    PARA-CSVDIR     TO  WRK-DIRECTORY
      *            INITIALIZE                  ORCSMKDIRAREA
      *            MOVE    WRK-DIRECTORY   TO  MKDIR-DIRECTORY
      *            MOVE    "0755"          TO  MKDIR-MODE
      *            MOVE    ZERO            TO  MKDIR-RESULT
      *            CALL    "orcmkdir"   USING  ORCSMKDIRAREA
      *            IF      MKDIR-RESULT NOT =  ZERO
      *                MOVE    "0"             TO  PARA-CSVKBN
      *            END-IF
      *        END-IF
      *    ELSE
      *        MOVE    "0"             TO  PARA-CSVKBN
      *        MOVE    SPACE           TO  WRK-DIRECTORY
      **** END-IF
      *
      *ver.4.7
      *    ＱＲ印刷判定
      *--- 2015/11/18 START ------------------------------------------
      *     IF      ORCHC62-QRKBN       =   SPACE
      *        患者設定のない時、オプション設定
      *         MOVE    PARA-QRKBN          TO  WRK-QRKBN
      *     ELSE
      *        患者設定の印刷あり
      *         IF      ORCHC62-QRKBN       =   "1"
      *             MOVE    "1"             TO  WRK-QRKBN
      *         END-IF
      *     END-IF
      *--- 2015/11/18 END   ------------------------------------------
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *--- 2015/11/18 START ------------------------------------------
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *
      *    コメント２
           MOVE    "COMMENT2"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-COMMENT2
      *H24.9
      *    ＱＲ印刷
      *--- 2015/11/18 START ------------------------------------------
      *     MOVE    "QRKBN"         TO  WRK-PARA-NAME
      *     PERFORM 1001-OPTION-CALL-SEC
      *     MOVE    WRK-PARA(1:1)   TO  PARA-QRKBN
           MOVE    "0"             TO  PARA-QRKBN
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    商品名記載
           MOVE    "SHOHIN"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-SHOHIN
      *
      *----(04.08.01)--ADD-START---
      *    処方せん備考欄コメント記載
           MOVE    "BIKOU"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-BIKOU
      *----(04.08.01)--ADD-END-----
      *
      *----(04.08.02)--ADD-START---
      *    効能効果・注意事項記載
           MOVE    "KOUNOU"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-KOUNOU
      *----(04.08.02)--ADD-END-----
      *
      *    注射記載
           MOVE    "TYUSYA"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-TYUSYA
           IF  WRK-PARA(1:1)         =   SPACE
               MOVE    "0"               TO  PARA-TYUSYA
           END-IF
      *
      *    検査記載
           MOVE    "KENSA"               TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-KENSA
           IF  WRK-PARA(1:1)         =   SPACE
               MOVE    "0"               TO  PARA-KENSA
           END-IF
      *
      *    画像記載
           MOVE    "GAZOU"               TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-GAZOU
           IF  WRK-PARA(1:1)         =   SPACE
               MOVE    "0"               TO  PARA-GAZOU
           END-IF
      *
      *--- 2016/03/02 START ------------------------------------------
      *    用法コメント
           MOVE    "YOUCOM"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF  WRK-PARA(1:9)         =   SPACE
               MOVE    SPACE             TO  PARA-YOUCOM
           ELSE
               MOVE    WRK-PARA(1:9)     TO  PARA-YOUCOM
           END-IF
      *--- 2016/03/02 END   ------------------------------------------
      *
      *--- 2016/09/26 START ------------------------------------------
      *    院内院外
           MOVE    "SHOHOU"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-SHOHOU
           IF  WRK-PARA(1:1)           =   SPACE
               MOVE    "1"               TO  PARA-SHOHOU
           END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *--- 2015/11/18 END   ------------------------------------------
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC        SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
      *--- 2015/11/18 START ------------------------------------------
      *     MOVE    "ORCHC62"       TO  ORCSPRGOPTION-PRGID
           MOVE    "A00000RA01"    TO  ORCSPRGOPTION-PRGID
      *--- 2015/11/18 END   ------------------------------------------
           MOVE    WRK-PARA-NAME   TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING
                                       SPA-AREA
                                       ORCSPRGOPTIONAREA
           MOVE    SPACE           TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *--- 2016/03/02 START ------------------------------------------
      **        半角で１文字目を値とする
      *         MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
               MOVE    ORCSPRGOPTION-OT-PARA(1:9)  TO  WRK-PARA
      *--- 2016/03/02 END   ------------------------------------------
           END-IF
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *--- 2015/11/18 START ------------------------------------------
      *****************************************************************
      *    主　　処理
      *****************************************************************
       201-MAIN-SEC                SECTION.
      *
      *--- 2016/04/06 START ------------------------------------------
           MOVE    ORCHC62-DRCD           TO  WRK-KEY-DRCD
      *--- 2016/04/06 END   ------------------------------------------
      *--- 2016/04/28 START ------------------------------------------
           MOVE    ORCHC62-DENPNUM        TO  WRK-KEY-DENPNUM
           MOVE    ORCHC62-CNT            TO  WRK-KEY-CNT
      *--- 2016/04/28 END   ------------------------------------------
      *
      *    発行画面区分
           EVALUATE  ORCHC62-PGKBN
               WHEN  2
      *              中途終了
                     PERFORM  210-MAIN-SEC
               WHEN  1
      *              請求確認
                     PERFORM  220-MAIN-SEC
               WHEN  3
      *              前回処方
                     PERFORM  230-MAIN-SEC
               WHEN  OTHER
                     GO TO  201-MAIN-EXT
           END-EVALUATE
      *
           .
       201-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了  主処理
      *****************************************************************
       210-MAIN-SEC                SECTION.
      *
      *    印刷対象取得処理
           PERFORM  210-SUB-SEC
      *
           PERFORM  VARYING  IDX-ST  FROM  1  BY  1
                    UNTIL  ( IDX-ST              >  5
                    OR       H016-DRCD1(IDX-ST)  =  SPACE )
      *
             INITIALIZE  ORCSCH62AREA
             INITIALIZE  ORCHC62AREA
      *
      *      中途終了時発行
      *--- 2016/09/26 START ------------------------------------------
      *       MOVE    "3"                     TO  ORCSCH62-KBN
             IF    PARA-SHOHOU          =   "1"
                 MOVE    "3"                 TO  ORCSCH62-KBN
             ELSE
                 MOVE    "7"                 TO  ORCSCH62-KBN
             END-IF
      *--- 2016/09/26 END   ------------------------------------------
             MOVE    H016-DRCD1(IDX-ST)      TO  SPA-DRCD
             MOVE    H016-DRCD2(IDX-ST)      TO  H016-DRCD
      *
             CALL    "A00000RA012"           USING
                                             SPA-AREA
                                             ORCSCH62AREA
                                             ORCHC62AREA
                                             H016-DR-AREA
      *
      *--- 2016/04/04 START ------------------------------------------
      *      ドクター名が一致していること
             IF  ORCHC62-DRCD         =   WRK-KEY-DRCD
      *--- 2016/04/04 END   ------------------------------------------
                 PERFORM 200-MAIN-SEC
      *--- 2016/04/04 START ------------------------------------------
             END-IF
      *--- 2016/04/04 END   ------------------------------------------
      *
           END-PERFORM
      *
           .
       210-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確認  主処理
      *****************************************************************
       220-MAIN-SEC                SECTION.
      *
      *--- 2016/04/28 START ------------------------------------------
      **    印刷対象取得処理
      *     PERFORM  220-SUB-SEC
      **
      *     PERFORM  VARYING  IDX-ST  FROM  1  BY  1
      *              UNTIL  ( IDX-ST  >  5
      *              OR       H016-DENPNUM(IDX-ST)  =  ZERO )
      *--- 2016/04/28 END   ------------------------------------------
      *
             INITIALIZE  ORCSCH62AREA
             INITIALIZE  ORCHC62AREA
      *
      *      請求確認時発行
      *--- 2016/09/26 START ------------------------------------------
      *       MOVE    "1"                     TO  ORCSCH62-KBN
             IF    PARA-SHOHOU          =   "1"
                 MOVE    "1"                 TO  ORCSCH62-KBN
             ELSE
                 MOVE    "6"                 TO  ORCSCH62-KBN
             END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *--- 2016/04/28 START ------------------------------------------
      *      伝票番号
      *       MOVE    H016-DENPNUM(IDX-ST)    TO  ORCSCH62-DENPNUM
             MOVE    WRK-KEY-DENPNUM         TO  ORCSCH62-DENPNUM
             MOVE    WRK-KEY-CNT             TO  ORCHC62-CNT
      *--- 2016/04/28 END   ------------------------------------------
      *
             CALL    "A00000RA012"           USING
                                             SPA-AREA
                                             ORCSCH62AREA
                                             ORCHC62AREA
                                             H016-DR-AREA
      *
      *--- 2016/04/04 START ------------------------------------------
      *      ドクター名が一致していること
      *--- 2016/04/28 START ------------------------------------------
      *       IF  ORCHC62-DRCD         =   WRK-KEY-DRCD
      *--- 2016/04/28 END   ------------------------------------------
      *--- 2016/04/04 END   ------------------------------------------
                 PERFORM 200-MAIN-SEC
      *--- 2016/04/04 START ------------------------------------------
      *--- 2016/04/28 START ------------------------------------------
      *       END-IF
      *--- 2016/04/28 END   ------------------------------------------
      *--- 2016/04/04 END   ------------------------------------------
      *
      *--- 2016/04/28 START ------------------------------------------
      *     END-PERFORM
      *--- 2016/04/28 END   ------------------------------------------
      *
           .
       220-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回処方  主処理
      *****************************************************************
       230-MAIN-SEC                SECTION.
      *
           PERFORM 200-MAIN-SEC
      *
           .
       230-MAIN-EXT.
           EXIT.
      *
      *--- 2015/11/18 END   ------------------------------------------
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票ＨＥＡＤ情報編集処理
           PERFORM 210-HEAD-HENSYU-SEC
      *
      *--- 2016/01/27 START ------------------------------------------
      *--- 2016/04/27 START ------------------------------------------
      *     IF      PARA-KOUNOU =   "0"  OR  "1"
           IF      PARA-KOUNOU =   "0"
      *--- 2016/04/27 END   ------------------------------------------
      *--- 2016/01/27 END   ------------------------------------------
      *      データがなくなるまで
             MOVE    ZERO            TO  CNT-ZAI
      *--- 2015/11/18 START ------------------------------------------
             MOVE    ZERO            TO  CNT-ZAI2
             MOVE    ZERO            TO  FLG-GROUP
             MOVE    ZERO            TO  FLG-SRYKBN2
             MOVE    ZERO            TO  FLG-SRYKBN3
             MOVE    ZERO            TO  FLG-SRYKBN6
             MOVE    ZERO            TO  FLG-SRYKBN7
      *--- 2015/11/18 END   ------------------------------------------
             PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                     UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *          帳票ＢＯＤＹ情報編集処理
                 PERFORM 220-BODY-HENSYU-SEC
             END-PERFORM
      *
      *--- 2015/11/18 START ------------------------------------------
             IF      FLG-GROUP       =   ZERO
                 GO  TO  200-MAIN-EXT
             END-IF
      *--- 2015/11/18 END   ------------------------------------------
      *
      *      データがなければ処理抜ける
             IF      CNT-ZAI         =   ZERO
                 GO  TO  200-MAIN-EXT
             END-IF
      *
      *      帳票ＦＯＯＴ情報編集処理
             PERFORM 430-FOOT-SET-SEC
      *
      *      帳票印刷処理
             PERFORM 400-PRT-WRITE-SEC
      *
      *--- 2016/01/27 START ------------------------------------------
           END-IF
      *--- 2016/01/27 END   ------------------------------------------
      *
      *
      *----(04.08.02)--ADD-START---
      *    プログラムオプションの設定があれば
      *--- 2016/01/27 START ------------------------------------------
      *     IF      PARA-KOUNOU =   "1"
      *--- 2016/04/27 START ------------------------------------------
      *     IF      PARA-KOUNOU =   "1"  OR  "2"
           IF      PARA-KOUNOU =   "1"
      *--- 2016/04/27 END   ------------------------------------------
      *--- 2016/01/27 END   ------------------------------------------
      *        効能効果・注意事項処理
               PERFORM 210-KOUNOU-MAIN-SEC
           END-IF
      *----(04.08.02)--ADD-END-----
      *
      *--- 2015/11/18 START ------------------------------------------
      *H24.9
      *    ＱＲ印刷
      *     IF     (WRK-QRKBN           =   "1"  )
      *        OR  (ORCHC62-CSVKBN  NOT =   SPACE )
      *         PERFORM 500-QR-SYORI-SEC
      *     END-IF
      *--- 2015/11/18 END   ------------------------------------------
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *----(04.08.02)--ADD-START---
      *****************************************************************
      *    効能効果・注意事項処理
      *****************************************************************
       210-KOUNOU-MAIN-SEC         SECTION.
      *
      *    帳票ＨＥＡＤセット処理
           INITIALIZE                  HCM62
           MOVE    ZERO            TO  CNT-LINE
           PERFORM 410-HEAD-SET-SEC
      *
      *    データがなくなるまで
           MOVE    ZERO            TO  CNT-ZAI
      *--- 2015/11/18 START ------------------------------------------
           MOVE    ZERO            TO  CNT-ZAI2
           MOVE    ZERO            TO  FLG-GROUP
           MOVE    ZERO            TO  FLG-SRYKBN2
           MOVE    ZERO            TO  FLG-SRYKBN3
           MOVE    ZERO            TO  FLG-SRYKBN6
           MOVE    ZERO            TO  FLG-SRYKBN7
      *--- 2015/11/18 END   ------------------------------------------
           PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                   UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *        帳票効能効果・注意事項情報編集処理
               PERFORM 220-KOUNOU-HENSYU-SEC
           END-PERFORM
      *
      *--- 2016/01/27 START ------------------------------------------
      *     IF      FLG-GROUP       =   ZERO
      *         GO  TO  210-KOUNOU-MAIN-EXT
      *     END-IF
      *--- 2016/01/27 END   ------------------------------------------
      *
      *    帳票ＦＯＯＴ情報編集処理
           PERFORM 430-FOOT-SET-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRT-WRITE-SEC
           .
       210-KOUNOU-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    帳票効能効果・注意事項情報編集処理
      *****************************************************************
       220-KOUNOU-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
           IF      ORCHC62-SRYKBN(IDX-ZAI) =   "98"
               GO  TO  220-KOUNOU-HENSYU-EXT
           END-IF
      *
           COMPUTE CNT-ZAI     =   CNT-ZAI     +   1
      *
      *--- 2015/11/18 START ------------------------------------------
      *    印字グループチェック
           PERFORM 220-BODY-GROUP-CHK-SEC
           IF  FLG-GROUP-PRT    =   1
               GO  TO  220-KOUNOU-HENSYU-EXT
           END-IF
      *--- 2015/11/18 END   ------------------------------------------
      *
      *--- 2016/02/08 START ------------------------------------------
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)
                                   TO  WRK-ZAIKAIKEI
           MOVE    ORCHC62-SRYSYUKBN(IDX-ZAI)
                                   TO  WRK-SRYSYUKBN
           MOVE    ORCHC62-SRYKBN(IDX-ZAI)
                                   TO  WRK-SRYKBN
      *--- 2016/02/08 END   ------------------------------------------
      *
      *--- 2015/11/18 START ------------------------------------------
      *    用法編集
           MOVE    1               TO  WRK-GROUPNAMEFLG
           MOVE    ZERO            TO  CNT-SRY
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
               PERFORM 230-KOUNOU-NAIYO-SEC
           END-PERFORM
      *    回数編集
           PERFORM 240-KAISUU-HENSYU-SEC
      *--- 2016/02/08 END   ------------------------------------------
      *
      *--- 2015/11/18 START ------------------------------------------
      *    グループ別連番の振り直し
           IF      ORCHC62-SRYKBN(IDX-ZAI)(1:1)   
                            NOT  =   ORCHC62-SRYKBN(IDX-ZAI - 1)(1:1)
               MOVE    ZERO            TO  CNT-ZAI2
           END-IF
      *--- 2015/11/18 END   ------------------------------------------
      *--- 2016/02/08 START ------------------------------------------
      *    内服・頓服・外用を分けて連番を振り直す
           IF  ORCHC62-SRYKBN(IDX-ZAI)(1:1)  =  "2"
               IF  ORCHC62-SRYKBN(IDX-ZAI) 
                                NOT = ORCHC62-SRYKBN(IDX-ZAI - 1)
                   MOVE    ZERO            TO  CNT-ZAI2
               END-IF
           END-IF
      *--- 2016/02/08 END   ------------------------------------------
      *
           MOVE    ZERO            TO  WRK-GROUPNAMEFLG
           MOVE    ZERO            TO  CNT-SRY
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-KOUNOU-NAIYO-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-KOUNOU-HENSYU-EXT
           END-IF
      *
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
           .
       220-KOUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-KOUNOU-NAIYO-SEC        SECTION.
      *
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *
      *    薬剤のみ編集
      *--- 2016/02/08 START ------------------------------------------
      *     IF      WRK-SRYCD(1:1)  NOT =   "6"
      *         GO  TO  230-KOUNOU-NAIYO-EXT
      *     END-IF
      *--- 2016/05/09 START ------------------------------------------
      *     IF      WRK-SRYCD(1:1)  NOT =   "6"
      *         IF  WRK-SRYCD(1:3)  NOT =   "001"
      *             GO  TO  230-KOUNOU-NAIYO-EXT
      *         END-IF
      *     END-IF
      *--- 2016/05/09 END   ------------------------------------------
      *--- 2016/02/08 END   ------------------------------------------
      *
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-KOUNOU-NAIYO-EXT
           END-IF
      *
      *--- 2015/11/18 START ------------------------------------------
      *    診療行為グループ名称印字
      *--- 2016/02/08 START ------------------------------------------
      *     PERFORM 231-GROUPMEI-SYORI-SEC
      *--- 2016/02/08 END   ------------------------------------------
      *--- 2015/11/18 END   ------------------------------------------
      *--- 2016/05/09 START ------------------------------------------
           IF      WRK-SRYCD(1:1)  NOT =   "6"
               IF  WRK-SRYCD(1:3)  NOT =   "001"
                   EVALUATE  ORCHC62-SRYKBN(IDX-ZAI)
                       WHEN  "60"
                       WHEN  "70"
                             IF    ( WRK-SRYCD(1:2)  NOT =   "16" )
                               AND ( WRK-SRYCD(1:2)  NOT =   "17" )
                                 GO  TO  230-KOUNOU-NAIYO-EXT
                             ELSE
                                 PERFORM 231-TENSUMEI-SYORI-SEC
                                 IF  WRK-CHARFLG          =   1
                                     GO  TO  230-KOUNOU-NAIYO-EXT
                                 END-IF
                             END-IF
                       WHEN  OTHER
                             GO  TO  230-KOUNOU-NAIYO-EXT
                   END-EVALUATE
               END-IF
           END-IF
      *--- 2016/05/09 END   ------------------------------------------
      *
      *    薬情マスタ検索
           PERFORM 900-YAKUJYO-READ-SEC
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
      *--- 2016/02/08 START ------------------------------------------
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYSURYO(CNT-SRY)
           MOVE    ORCHC62-SRYKAISU(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYKAISU(CNT-SRY)
           MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                   TO  WRK-INPUTCOMENT(CNT-SRY)
           MOVE    ORCHC62-YOURYOU-TBL(IDX-ZAI IDX-SRY)
                                   TO  WRK-YOURYOU-TBL(CNT-SRY)
      *
           INITIALIZE                  WRK-HEN-AREA
      *--- 2016/02/08 END   ------------------------------------------
      *
      *--- 2016/01/26 START ------------------------------------------
      *    グループ連番印字
      *--- 2016/02/08 START ------------------------------------------
      *     PERFORM 231-GROUP-REN-SEC
      *--- 2016/02/08 END   ------------------------------------------
      *--- 2016/01/26 END   ------------------------------------------
      *
      *    １件目だけ番号表示
           IF      CNT-SRY         =   1
      *--- 2015/11/18 START ------------------------------------------
      *         MOVE    CNT-ZAI         TO  WRK-Z9
      *--- 2016/02/08 START ------------------------------------------
      *         IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "31"
      *                                          OR "32"
      *                                          OR "33"
      *                                          OR "60"
      *                                          OR "70"
      *                 MOVE    CNT-ZAI2        TO  WRK-Z9
      *         ELSE
      *                 MOVE    CNT-ZAI         TO  WRK-Z9
      *         END-IF
      *         COMPUTE CNT-ZAI2         =  CNT-ZAI2    +   1
      *         MOVE    CNT-ZAI2        TO  WRK-Z9
      *--- 2016/02/08 END   ------------------------------------------
      *--- 2015/11/18 END   ------------------------------------------
      *         MOVE    WRK-Z9          TO  WRK-HENLINE(1:2)
      *         MOVE    ")"             TO  WRK-HENLINE(3:1)
      *
               IF  WRK-GROUPNAMEFLG      =   ZERO
                 COMPUTE CNT-ZAI2        =   CNT-ZAI2    +   1
                 MOVE    CNT-ZAI2        TO  WRK-Z9
                 MOVE    WRK-Z9          TO  WRK-HENLINE(1:2)
                 MOVE    ")"             TO  WRK-HENLINE(3:1)
               ELSE
      *         グループ名称印字
                 PERFORM 231-GROUPMEI-SYORI3-SEC
               END-IF
      *
           ELSE
               IF  WRK-GROUPNAMEFLG      =   ZERO
               MOVE    WRK-LINE-SEN2   TO  HCM62-SEN(CNT-LINE)
               END-IF
           END-IF
      *
      *    薬剤名編集
           MOVE    SPACE           TO  WRK-HENSYUDATA
      *--- 2016/02/08 START ------------------------------------------
      *     MOVE    TNS-NAME        TO  WRK-HENSYUDATA
      *     PERFORM 250-HENSYU-SYORI-SEC
      *--- 2016/05/09 START ------------------------------------------
      *     IF  WRK-GROUPNAMEFLG      =   1
      *         IF      TNS-SRYCD(1:3)      =   "001"
      **          用法編集
      *           PERFORM 240-YOUHOU-HENSYU-SEC
      *           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
      *         END-IF
      *     END-IF
      *--- 2016/05/09 END   ------------------------------------------
      *--- 2016/05/09 START ------------------------------------------
      *     IF  WRK-GROUPNAMEFLG      =   ZERO
      *       IF      TNS-SRYCD(1:1)      =   "6"
           EVALUATE  TRUE
               WHEN  WRK-SRYCD(1:1)     =   "6"
                   IF  WRK-GROUPNAMEFLG      =   ZERO
      *--- 2016/05/09 END   ------------------------------------------
      *                名称編集
                       PERFORM 240-YKZNAME-HENSYU-SEC
      *                数量編集
                       PERFORM 240-SURYOU-HENSYU-SEC
      *                効能編集処理
                       IF      YAKUJYO-EFFECT  NOT =   SPACE
                           MOVE    SPACE           TO  WRK-HENSYUDATA
                           MOVE    "【効能効果】"  TO  WRK-HENSYUDATA
                           PERFORM 250-HENSYU-SYORI-SEC
                           MOVE    SPACE           TO  WRK-HENSYUDATA
                           MOVE    YAKUJYO-EFFECT  TO  WRK-HENSYUDATA
                           PERFORM 250-HENSYU-SYORI-SEC
                       END-IF
      *                注意事項編集処理
                       IF      YAKUJYO-CAUTION NOT =   SPACE
                           MOVE    SPACE           TO  WRK-HENSYUDATA
                           MOVE    "【注意事項】"  TO  WRK-HENSYUDATA
                           PERFORM 250-HENSYU-SYORI-SEC
                           MOVE    SPACE           TO  WRK-HENSYUDATA
                           MOVE    YAKUJYO-CAUTION TO  WRK-HENSYUDATA
                           PERFORM 250-HENSYU-SYORI-SEC
                       END-IF
                   END-IF
      *--- 2016/05/09 START ------------------------------------------
      *     END-IF
               WHEN  WRK-SRYCD(1:2)     =   "16"
               WHEN  WRK-SRYCD(1:2)     =   "17"
                     IF  WRK-GROUPNAMEFLG    =   ZERO
                         MOVE    SPACE           TO  WRK-HENSYUDATA
                         MOVE    TNS-NAME        TO  WRK-HENSYUDATA
                         PERFORM 250-HENSYU-SYORI-SEC
                     END-IF
               WHEN  WRK-SRYCD(1:3)     =   "001"
                     IF  WRK-GROUPNAMEFLG    =   1
                       PERFORM 240-YOUHOU-HENSYU-SEC
                       MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
                     END-IF
           END-EVALUATE
      *--- 2016/05/09 END   ------------------------------------------
      *--- 2016/02/08 END   ------------------------------------------
      *
           .
       230-KOUNOU-NAIYO-EXT.
           EXIT.
      *----(04.08.02)--ADD-END-----
      *****************************************************************
      *    帳票ＨＥＡＤ情報編集処理
      *****************************************************************
       210-HEAD-HENSYU-SEC         SECTION.
      *
      *    診療日
           MOVE    ORCHC62-SRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3-G  TO  WRK-SRYYMDG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-SRYYMDY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-SRYYMDM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-SRYYMDD
      *
      *    患者氏名
           MOVE    SPACE           TO  WRK-PTNAME
           IF      SPA-NAME        NOT =   SPACE
               STRING  SPA-NAME        DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           ELSE
               STRING  SPA-KANANAME    DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           END-IF
           INSPECT WRK-PTNAME          REPLACING  ALL "  "  BY  "　"
      *
      *    帳票ＨＥＡＤセット処理
           INITIALIZE                  HCM62
           MOVE    ZERO            TO  CNT-LINE
           PERFORM 410-HEAD-SET-SEC
      *
           .
       210-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ情報編集処理
      *****************************************************************
       220-BODY-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
      *----(04.08.01)--UPD-START---
      *    処方せん備考欄コメント表示しない場合
           IF      PARA-BIKOU      =   "0"
               IF      ORCHC62-SRYKBN(IDX-ZAI) =   "98"
                   GO  TO  220-BODY-HENSYU-EXT
               END-IF
           END-IF
      *----(04.08.01)--UPD-END-----
      *
           COMPUTE CNT-ZAI     =   CNT-ZAI     +   1
      *
      *--- 2015/11/18 START ------------------------------------------
      *    印字グループチェック
           PERFORM 220-BODY-GROUP-CHK-SEC
           IF  FLG-GROUP-PRT    =   1
                   GO  TO  220-BODY-HENSYU-EXT
           END-IF
      *
      *    グループ別連番の振り直し
           IF  ORCHC62-SRYKBN(IDX-ZAI)(1:1)
                                NOT = ORCHC62-SRYKBN(IDX-ZAI - 1)(1:1)
               MOVE    ZERO            TO  CNT-ZAI2
           END-IF
      *--- 2015/11/18 END   ------------------------------------------
      *--- 2016/02/08 START ------------------------------------------
      *    内服・頓服・外用を分けて連番を振り直す
           IF  ORCHC62-SRYKBN(IDX-ZAI)(1:1)  =  "2"
               IF  ORCHC62-SRYKBN(IDX-ZAI) 
                                NOT = ORCHC62-SRYKBN(IDX-ZAI - 1)
                   MOVE    ZERO            TO  CNT-ZAI2
               END-IF
           END-IF
      *--- 2016/02/08 END   ------------------------------------------
      *
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
      *
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)
                                   TO  WRK-ZAIKAIKEI
           MOVE    ORCHC62-SRYSYUKBN(IDX-ZAI)
                                   TO  WRK-SRYSYUKBN
           MOVE    ORCHC62-SRYKBN(IDX-ZAI)
                                   TO  WRK-SRYKBN
      *!
           MOVE    SPACE           TO  WRK-SSTKIJUNCD
           IF      ORCHC62-YSRYCD(IDX-ZAI) NOT =   SPACE
               MOVE    ORCHC62-YSRYCD(IDX-ZAI) TO  WRK-SRYCD
      *        点数検索
               PERFORM 900-TENSU-READ-SEC
      *        回数カウント
               MOVE    ZERO            TO  CNT-KAISU
               PERFORM VARYING IDX-KBN     FROM    1   BY  1
                       UNTIL   IDX-KBN     >   5
                   IF      TNS-SSTKIJUNCD(IDX-KBN) =   1
                       COMPUTE CNT-KAISU   =   CNT-KAISU   +   1
                       MOVE    "1"         TO  WRK-SSTKIJUNCD(IDX-KBN:1)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    ZERO            TO  CNT-SRY
      *H24.9
      *--- 2015/11/18 START ------------------------------------------
      *    ＱＲ対応
      *     MOVE    ZERO                TO  IDX-QR2
      *--- 2015/11/18 END   ------------------------------------------
      *
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-NAIYO-HENSYU-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-BODY-HENSYU-EXT
           END-IF
      *
      *    回数編集
           PERFORM 240-KAISUU-HENSYU-SEC
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-NAIYO-HENSYU-SEC        SECTION.
      *
      *ver.4.7
      *    商品名コード
           MOVE    SPACE               TO  WRK-SHO-TANINAME
           MOVE    SPACE               TO  WRK-SHO-NAME
           IF     (PARA-SHOHIN         =   "1"  )
             AND  (ORCHC62-SHO-SRYCD(IDX-ZAI IDX-SRY)
                                   NOT =   SPACE )
               MOVE    ORCHC62-SHO-SRYCD (IDX-ZAI IDX-SRY)
                                           TO  WRK-SRYCD
      *        点数検索
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANINAME        TO  WRK-SHO-TANINAME
               MOVE    TNS-NAME            TO  WRK-SHO-NAME
           END-IF
      *
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *     IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "001" )
      *         GO  TO  230-NAIYO-HENSYU-EXT
      *     END-IF
      *
      *    減の文字は対象外
           IF      WRK-SRYCD   =   "820000047"
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
      *--- 2015/11/18 START ------------------------------------------
      *--- 2016/03/02 START ------------------------------------------
      **    検査 画像の診療行為名称のに「〜加算」「〜判断料」などは印字しない
      *     PERFORM 231-TENSUMEI-SYORI-SEC
      *     IF  WRK-CHARFLG          =   1
      *         GO  TO  230-NAIYO-HENSYU-EXT
      *     END-IF
           EVALUATE  ORCHC62-SRYKBN(IDX-ZAI)
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
      *              注射
                     IF  WRK-SRYCD(1:1)  =   "6"
                         CONTINUE
                     ELSE
                         IF    WRK-SRYCD(1:4)  =   "0086"
                           IF  WRK-SRYCD     NOT =   PARA-YOUCOM
                               GO  TO  230-NAIYO-HENSYU-EXT
                           END-IF
                         ELSE
                           GO  TO  230-NAIYO-HENSYU-EXT
                         END-IF
                     END-IF
               WHEN  "60"
               WHEN  "70"
      *              検査  画像診断
                     IF  WRK-SRYCD(1:1)  =   "6"
                         GO  TO  230-NAIYO-HENSYU-EXT
                     ELSE
                         IF  WRK-SRYCD(1:2)  =   "16"  OR  "17"
      *                      検査、画像診断は
      *                      診療行為名の「〜加算」「〜判断料」などは印字しない
                             PERFORM 231-TENSUMEI-SYORI-SEC
                             IF  WRK-CHARFLG          =   1
                                 GO  TO  230-NAIYO-HENSYU-EXT
                             END-IF
                         ELSE
                             IF    WRK-SRYCD(1:4)  =   "0086"
                               IF  WRK-SRYCD     NOT =   PARA-YOUCOM
                                   GO  TO  230-NAIYO-HENSYU-EXT
                               END-IF
                             ELSE
                               GO  TO  230-NAIYO-HENSYU-EXT
                             END-IF
                         END-IF
                     END-IF
               WHEN  OTHER
                     IF    WRK-SRYCD(1:4)  =   "0086"
                       IF  WRK-SRYCD     NOT =   PARA-YOUCOM
                           GO  TO  230-NAIYO-HENSYU-EXT
                       END-IF
                     END-IF
      *
           END-EVALUATE
      *--- 2016/03/02 END   ------------------------------------------
      *
      *--- 2016/02/08 START ------------------------------------------
      **    グループ名称印字
      *     PERFORM 231-GROUPMEI-SYORI-SEC
      *--- 2016/02/08 END   ------------------------------------------
      *--- 2015/11/18 END   ------------------------------------------
      *
      *ver.4.7
      *    商品名コード
           IF      ORCHC62-SHO-SRYCD(IDX-ZAI IDX-SRY)  NOT =   SPACE
      *        名称と単位は商品名コードの内容とする
               IF      WRK-SHO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SHO-TANINAME    TO  TNS-TANINAME
               END-IF
               IF      WRK-SHO-NAME        NOT =   SPACE
                   MOVE    WRK-SHO-NAME        TO  TNS-NAME
               END-IF
           END-IF
      *!
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYSURYO(CNT-SRY)
           MOVE    ORCHC62-SRYKAISU(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYKAISU(CNT-SRY)
           MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                   TO  WRK-INPUTCOMENT(CNT-SRY)
           MOVE    ORCHC62-YOURYOU-TBL(IDX-ZAI IDX-SRY)
                                   TO  WRK-YOURYOU-TBL(CNT-SRY)
      *
           INITIALIZE                  WRK-HEN-AREA
      *
      *    ＱＲ用
           MOVE    SPACE               TO  WRK-TNS-NAME
           MOVE    SPACE               TO  WRK-YOH-NAME-G
           MOVE    ZERO                TO  IDX-QY
      *
      *--- 2016/01/26 START ------------------------------------------
      *    グループ連番印字
      *--- 2016/02/08 START ------------------------------------------
      *     PERFORM 231-GROUP-REN-SEC
      *--- 2016/02/08 END   ------------------------------------------
      *--- 2016/01/26 END   ------------------------------------------
      *
      *----(04.08.01)--UPD-START---
           IF      ORCHC62-SRYKBN(IDX-ZAI) NOT =   "98"
      *        １件目だけ番号表示
               IF      CNT-SRY         =   1
      *--- 2015/11/18 START ------------------------------------------
      *             MOVE    CNT-ZAI         TO  WRK-Z9
      *--- 2016/02/08 START ------------------------------------------
      *             IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "31"
      *                                              OR "32"
      *                                              OR "33"
      *                                              OR "60"
      *                                              OR "70"
      *                     MOVE    CNT-ZAI2        TO  WRK-Z9
      *             ELSE
      *                     MOVE    CNT-ZAI         TO  WRK-Z9
      *             END-IF
                   COMPUTE CNT-ZAI2         =  CNT-ZAI2    +   1
                   MOVE    CNT-ZAI2        TO  WRK-Z9
      *--- 2016/02/08 END   ------------------------------------------
      *--- 2015/11/18 END   ------------------------------------------
                   MOVE    WRK-Z9          TO  WRK-HENLINE(1:2)
                   MOVE    ")"             TO  WRK-HENLINE(3:1)
      *--- 2016/02/08 START ------------------------------------------
      *            グループ名称印字
                   PERFORM 231-GROUPMEI-SYORI2-SEC
      *--- 2016/02/08 END   ------------------------------------------
               END-IF
           END-IF
      *----(04.08.01)--UPD-END-----
      *
           IF      TNS-SRYCD(1:3)  =   "001"
      *        用法編集
               PERFORM 240-YOUHOU-HENSYU-SEC
           ELSE
      *        名称編集
               PERFORM 240-YKZNAME-HENSYU-SEC
      *        数量編集
               PERFORM 240-SURYOU-HENSYU-SEC
               IF     (PARA-COMMENT2   =   "1"  )
               AND    (WRK-SRYCD(1:1)  =   "6"  )
      *            用量編集
                   PERFORM 240-YOURYOU-HENSYU-SEC
               END-IF
           END-IF
      *
      *--- 2015/11/18 START ------------------------------------------
      *H24.9
      *    ＱＲ名称編集
      *     PERFORM 5001-QRMEISAI-HENSYU-SEC
      *--- 2015/11/18 END   ------------------------------------------
      *
           .
       230-NAIYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法編集処理
      *****************************************************************
       240-YOUHOU-HENSYU-SEC       SECTION.
      *
      *--- 2016/02/08 START ------------------------------------------
           MOVE    SPACE           TO  WRK-YOUHOU
      *--- 2016/02/08 END   ------------------------------------------
      *
      *    コメント名称判定
           IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
               STRING  "【"            DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           ELSE
               STRING  "【"            DELIMITED   BY  SIZE
                       WRK-INPUTCOMENT(CNT-SRY)
                                       DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           END-IF
      *    ＱＲ用
           MOVE    WRK-YOUHOU      TO  WRK-TNS-NAME
      *
      *--- 2016/02/16 START ------------------------------------------
      *     MOVE    WRK-YOUHOU      TO  WRK-HENSYUDATA
           STRING  "　"                DELIMITED   BY  SIZE
                   WRK-YOUHOU          DELIMITED   BY  SIZE
                   INTO                WRK-HENSYUDATA
           END-STRING
      *--- 2016/02/16 END   ------------------------------------------
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YOUHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       240-YKZNAME-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-YKZNAME
      *
      *    薬剤編集
           IF      TNS-SRYCD(1:1)  =   "6"
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   MOVE    1               TO  FLG-SOURYO
               END-IF
               MOVE    TNS-NAME            TO  WRK-YKZNAME
           ELSE
      *        コメントあればセット、なければ名称セット
               IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
                   MOVE    TNS-NAME        TO  WRK-YKZNAME
               ELSE
                   MOVE    WRK-INPUTCOMENT(CNT-SRY)
                                           TO  WRK-YKZNAME
               END-IF
           END-IF
      *    ＱＲ用
           MOVE    WRK-YKZNAME     TO  WRK-TNS-NAME
      *
           MOVE    WRK-YKZNAME     TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YKZNAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       240-SURYOU-HENSYU-SEC       SECTION.
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *    IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *          ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *          ( WRK-SRYCD(1:3)  NOT =   "001" )
      *        GO  TO  240-SURYOU-HENSYU-EXT
      *****END-IF
      *
      *    薬剤、器材のみ
           IF    ( WRK-SRYCD(1:1)      =   "6"   )  OR
                 ( WRK-SRYCD(1:1)      =   "7"   )  OR
                 ( WRK-SRYCD(1:3)      =   "059" )
               CONTINUE
           ELSE
               GO  TO  240-SURYOU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  WRK-SURYOU
           END-IF
      *
           STRING  " "             DELIMITED  BY  SIZE
                   WRK-SURYOU      DELIMITED  BY  SPACE
                   " "             DELIMITED  BY  SIZE
                   TNS-TANINAME    DELIMITED  BY  SPACE
                   INTO            WRK-HENNAIYO
           END-STRING
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-SURYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集処理
      *****************************************************************
       240-KAISUU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF    ( WRK-SRYKBN      =   "21" )  AND
                 ( FLG-SOURYO      =   1    )
               MOVE    "23"            TO  WRK-SRYKBN
           END-IF
      *
      *    内服か頓服でなければ処理抜ける
           IF    ( WRK-SRYKBN  NOT =   "21" )  AND
                 ( WRK-SRYKBN  NOT =   "22" )
               GO  TO  240-KAISUU-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-ZAIKAIKEI   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-KAISUU
           END-IF
      *
           EVALUATE    WRK-SRYKBN
      *        内服
               WHEN    "21"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "日分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *        頓服
               WHEN    "22"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "回分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
           END-EVALUATE
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-KAISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用量編集処理
      *****************************************************************
       240-YOURYOU-HENSYU-SEC       SECTION.
      *
      *    用量コード設定分セット
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           MOVE    CNT-KAISU       TO  WRK-KAISU
           MOVE    ZERO            TO  WRK-HENKBN-G
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
                   OR    ( FLG-KBN     =   1 )
      *        用量コードがあった場合
               IF      WRK-YOURYOU(CNT-SRY IDX-KBN)     >   ZERO
      *            指定してない服用時点ならエラー
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   NOT  =   "1"
                       MOVE    1               TO  FLG-KBN
                   ELSE
      *                用量の方が多い場合エラー
                       IF      WRK-SURYO      <
                                   WRK-YOURYOU(CNT-SRY IDX-KBN) 
                           MOVE    1               TO  FLG-KBN
                       ELSE
                           MOVE    WRK-YOURYOU(CNT-SRY IDX-KBN) 
                                               TO  WRK-HENKBN(IDX-KBN)
      *                    用量コード使用分は計算にいれない
                           COMPUTE WRK-SURYO  =   WRK-SURYO
                               -   WRK-YOURYOU(CNT-SRY IDX-KBN) 
                           COMPUTE WRK-KAISU   =   WRK-KAISU   -   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    エラーなら処理抜ける
           IF      FLG-KBN  =   1
               GO  TO  240-YOURYOU-HENSYU-EXT
           END-IF
      *
      *    入力分をセット
           COMPUTE WRK-YOURYO   =   WRK-SURYO  /   WRK-KAISU
      *    割切れない場合処理抜ける
           IF      WRK-SURYO    =   WRK-YOURYO *   WRK-KAISU
               PERFORM VARYING  IDX-KBN    FROM    1   BY  1
                       UNTIL  ( IDX-KBN    >   5 )
      *            服用時点コードで設定していない場合は割ったものをセット
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   =   "1"
                       IF      WRK-HENKBN(IDX-KBN)     =   0
                           MOVE    WRK-YOURYO  TO  WRK-HENKBN(IDX-KBN)
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               GO  TO  240-YOURYOU-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
      *        用量コードがあった場合
               IF      WRK-HENKBN(IDX-KBN)     >   ZERO
                   MOVE    SPACE       TO  WRK-HENSYUDATA
      *
                   EVALUATE    IDX-KBN
                       WHEN    1
                           MOVE    "　朝："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    2
                           MOVE    "　昼："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    3
                           MOVE    "　夕："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    4
                           MOVE    "　寝："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    5
                           MOVE    "　起："    TO  WRK-HENSYUDATA(1:6)
                   END-EVALUATE
      *
                   MOVE    WRK-HENKBN(IDX-KBN)
                                               TO  WRK-SURYO
                   PERFORM 250-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO        TO  WRK-SURYOU
      *
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-SURYOU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           TNS-TANINAME    DELIMITED  BY  SPACE
                           INTO            WRK-HENSYUDATA(7:)
                   END-STRING
      *H24.9
      *            ＱＲ　飲み方コメント
                   ADD     1                   TO  IDX-QY
                   MOVE    WRK-HENSYUDATA      TO  WRK-YOH-NAME(IDX-QY)
      *
                   PERFORM 250-HENSYU-SYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       240-YOURYOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    頁チェック　処理
      *****************************************************************
       250-PAGE-CHK-SEC            SECTION.
      *
      *    帳票の最大行数
           MOVE    34                  TO  CNT-LINE-MAX
      *
      *    ページ件数を超えていたら印刷
      *--- 2015/11/18 START ------------------------------------------
      *     IF      CNT-LINE    =   68
           IF      CNT-LINE    >=   CNT-LINE-MAX
      *
      *--- 2015/11/18 END   ------------------------------------------
               PERFORM 400-PRT-WRITE-SEC
               INITIALIZE                  HCM62
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 410-HEAD-SET-SEC
      *--- 2015/11/18 START ------------------------------------------
      *     ELSE
      *        ページ件数を超えていたら改ページ
      *         IF      CNT-LINE    =   34
      *             PERFORM 410-HEAD-SET-SEC
      *         END-IF
      *--- 2015/11/18 END   ------------------------------------------
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           .
       250-PAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO            TO  FLG-SP
           PERFORM VARYING IDX-STR     FROM    5   BY  -1
                   UNTIL   IDX-STR     <   1
               IF      WRK-SURYO-S2(IDX-STR:1) NOT =   ZERO
                   MOVE    1           TO  FLG-SP
               END-IF
               IF      FLG-SP      =   1
                   MOVE    WRK-SURYO-S2(IDX-STR:1)
                                        TO  WRK-SURYO-Z3(IDX-STR:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   11
               IF      WRK-SURYO-X(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE  WRK-SURYO-X(IDX-STR:1)
                                           TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-MHEN-SYORI-SET-SEC      SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    0              TO  IDX-STR1
      *
      *    最大文字を超えるか編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   23 )
                   OR     (WRK-HENNAIYO(IDX-STR:)    =   SPACE)
               ADD     1               TO  IDX-STR
               ADD     1               TO  IDX-STR1
           END-PERFORM
      *
      *--- 2015/11/18 START ------------------------------------------
      *     COMPUTE IDX-STR     =   54  -   IDX-STR1
           COMPUTE IDX-STR1    =   IDX-STR1  +  4
           COMPUTE IDX-STR     =   48  -   IDX-STR1
      *--- 2015/11/18 END   ------------------------------------------
      *
      *    文字が重なる場合は１行下に表示する
           IF    ( HCM62-NAIYO2(CNT-LINE)              NOT =   SPACE )
           OR    ( HCM62-NAIYO1(CNT-LINE)(IDX-STR:)    NOT =   SPACE )
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           ELSE
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           END-IF
      *
           .
       250-MHEN-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    4              TO  IDX-STR1
      *
      *----(04.08.02)--UPD-START---
      *    編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   4000 )
      *--- 2015/11/18 START ------------------------------------------
      *             OR     (IDX-STR1    >   54 )
                   OR     (IDX-STR1    >   48 )
      *--- 2015/11/18 END   ------------------------------------------
                   OR     (WRK-HENSYUDATA(IDX-STR:)    =   SPACE)
      *        改行
               IF      WRK-HENSYUDATA(IDX-STR:1)   =   X"0A"
                   ADD     1               TO  IDX-STR
               ELSE
                   MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                       TO  WRK-HENLINE(IDX-STR1:2)
                   ADD     2               TO  IDX-STR
                   ADD     2               TO  IDX-STR1
               END-IF
      *        最大字で改行
      *--- 2015/11/18 START ------------------------------------------
      *         IF         (IDX-STR1        >=  54)
               IF         (IDX-STR1        >=  48)
      *--- 2015/11/18 END   ------------------------------------------
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-HENSYUDATA(IDX-STR:) =   SPACE))
                   PERFORM 250-PAGE-CHK-SEC
                   MOVE    WRK-HENLINE     TO  HCM62-NAIYO1(CNT-LINE)
                   MOVE    SPACE           TO  WRK-HENLINE
                   MOVE    4               TO  IDX-STR1
               END-IF
           END-PERFORM
      *----(04.08.02)--UPD-END-----
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *ver.4.7
      *    ＣＳＶ出力のみは印刷なし
           IF      ORCHC62-PRTKBN      =   "1"
               GO      TO      400-PRT-WRITE-EXT
           END-IF
      *
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHC62-CNT     TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
      *
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
      *--- 2015/11/18 START ------------------------------------------
      *     MOVE    "HCM62.red"     TO  MKPRT-DIA
           MOVE    "A00000RA01.red" TO  MKPRT-DIA
      *--- 2015/11/18 END   ------------------------------------------
           MOVE    SPACE           TO  MKPRT-DEF
      *****MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(16)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
      *--- 2015/11/18 START ------------------------------------------
      *     CALL   "ORCSMKPRT1"         USING
           CALL   "ORCSMKPRTSITE1"     USING
      *--- 2015/11/18 END   ------------------------------------------
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       410-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62-NAIYO1(CNT-LINE)
      *
      *--- 2015/11/18 START ------------------------------------------
      *     MOVE    SPACE           TO  HCM62-NAIYO2(CNT-LINE)
      *     STRING  WRK-ZZ9         DELIMITED  BY  SIZE
      *             " 頁"           DELIMITED  BY  SIZE
      *             INTO            HCM62-NAIYO2(CNT-LINE)
      *     END-STRING
      *--- 2015/11/18 END   ------------------------------------------
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *--- 2015/11/18 START ------------------------------------------
      *     MOVE    WRK-LINE-2      TO  HCM62-NAIYO1(CNT-LINE)
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-LINE-2      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  HCM62-NAIYO1(CNT-LINE)
      *--- 2015/11/18 END   ------------------------------------------
      *
      *--- 2015/11/18 START ------------------------------------------
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-DRNAME TO  HCM62-NAIYO1(CNT-LINE)
      *--- 2015/11/18 END   ------------------------------------------
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
      *
           .
       410-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＦＯＯＴセット処理
      *****************************************************************
       430-FOOT-SET-SEC            SECTION.
      *
      *    医療機関名称セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-3      TO  HCM62-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           PERFORM 250-PAGE-CHK-SEC
      *--- 2015/11/18 START ------------------------------------------
      *     MOVE    WRK-LINE-4      TO  HCM62-NAIYO1(CNT-LINE)
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-LINE-4      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  HCM62-NAIYO1(CNT-LINE)
      *--- 2015/11/18 END   ------------------------------------------
      *
           .
       430-FOOT-SET-EXT.
           EXIT.
      *
      *H24.9
      *****************************************************************
      *    ＱＲ剤編集処理
      *****************************************************************
       5001-QRMEISAI-HENSYU-SEC            SECTION.
      *
           IF      ORCHC62-SRYKBN (IDX-ZAI)    =   "98"
      *        備考コメント
               PERFORM 50012-QR-BIKOU-HEN-SEC
           ELSE
      *        ＱＲ剤編集
               PERFORM 50011-QR-MEISAI-HEN-SEC
           END-IF
      *
           .
       5001-QRMEISAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ剤編集処理
      *****************************************************************
       50011-QR-MEISAI-HEN-SEC            SECTION.
      *
           IF      IDX-QR2             =   ZERO
               ADD     1                   TO  IDX-QR1
      *
      *        剤回数
               MOVE    ORCHC62-ZAIKAIKEI (IDX-ZAI)
                                       TO  ORCSQRCSV62-KAISU (IDX-QR1)
      *        診療区分
               MOVE    ORCHC62-SRYKBN (IDX-ZAI)
                                       TO  ORCSQRCSV62-SRYKBN (IDX-QR1)
      *        診療種別
               MOVE    ORCHC62-SRYSYUKBN (IDX-ZAI)
                                       TO  ORCSQRCSV62-SRYSYUKBN
                                                              (IDX-QR1)
           END-IF
      *
           ADD     1                   TO  IDX-QR2
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                       TO  ORCSQRCSV62-SRYCD
                                                       (IDX-QR1 IDX-QR2)
           MOVE    WRK-TNS-NAME        TO  ORCSQRCSV62-TNS-NAME
                                                       (IDX-QR1 IDX-QR2)
           MOVE    TNS-TANINAME        TO  ORCSQRCSV62-TANINAME
                                                       (IDX-QR1 IDX-QR2)
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                       TO  ORCSQRCSV62-SURYOU
                                                       (IDX-QR1 IDX-QR2)
           MOVE    TNS-YKZKBN          TO  ORCSQRCSV62-YKZKBN
                                                       (IDX-QR1 IDX-QR2)
           IF      TNSP-SOURYOHENKBN   =   1
      *        内服点剤
               MOVE    "1"             TO  ORCSQRCSV62-NAIHENKBN
                                                       (IDX-QR1 IDX-QR2)
           END-IF
           IF      ORCHC62-SRYCD(IDX-ZAI IDX-SRY)(1:3) =   "001"
      *        用法コメント区分
               MOVE    TNS-KENTAICOMMENT   TO  ORCSQRCSV62-YCOMMNTKBN
                                                  (IDX-QR1 IDX-QR2)
           END-IF
      *
      *    飲み方コメント
           PERFORM VARYING     IDX-QY  FROM    1   BY  1
                   UNTIL      (IDX-QY      >   5   )
                           OR (WRK-YOH-NAME (IDX-QY)   =   SPACE )
                           OR (IDX-QR2     >=   50   )
               ADD     1                   TO  IDX-QR2
               MOVE    "810000001"         TO  ORCSQRCSV62-SRYCD
                                                       (IDX-QR1 IDX-QR2)
               MOVE    WRK-YOH-NAME-2(IDX-QY)
                                           TO  ORCSQRCSV62-TNS-NAME
                                                       (IDX-QR1 IDX-QR2)
           END-PERFORM
      *
           .
       50011-QR-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ備考コメント処理
      *****************************************************************
       50012-QR-BIKOU-HEN-SEC            SECTION.
      *
           ADD     1                   TO  IDX-QRB
           IF      IDX-QRB              <=  10
               MOVE    WRK-TNS-NAME        TO  ORCSQRCSV62-BIKOU
                                                       (IDX-QRB)
           END-IF
           .
       50012-QR-BIKOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ印刷処理
      *****************************************************************
       500-QR-SYORI-SEC            SECTION.
      *
           MOVE    ORCHC62-SRYYMD      TO  ORCSQRCSV62-SRYYMD
      *    医療機関情報
           MOVE    SYS-1001-PREFNUM    TO  ORCSQRCSV62-PREFNUM
           MOVE    SYS-1001-HOSPCD     TO  ORCSQRCSV62-HOSPCD
           MOVE    SYS-1001-HOSPSBT    TO  ORCSQRCSV62-HOSPSBT
           MOVE    WRK-HOSPNAME        TO  ORCSQRCSV62-HOSPNAME
      *    ドクター
           MOVE    SYS-1010-KANANAME   TO  ORCSQRCSV62-DR-KANANAME
           MOVE    SYS-1010-NAME       TO  ORCSQRCSV62-DR-NAME
           MOVE    ORCHC62-DRCD        TO  ORCSQRCSV62-DRCD
      *    患者情報
           MOVE    SPA-NAME            TO  ORCSQRCSV62-NAME
           MOVE    SPA-KANANAME        TO  ORCSQRCSV62-KANANAME
           MOVE    SPA-SEX             TO  ORCSQRCSV62-SEX
           MOVE    SPA-BIRTHDAY        TO  ORCSQRCSV62-BIRTHDAY
      *    時間
           COMPUTE WRK-SYS-TIME        =   SYS-TIME
                                       +   ORCHC62-CNT
      *    ファイル名
           MOVE    CONS-FORM-ID        TO  ORCSQRCSV62-FORM-ID
      *    ファイル名組み立て項目
           MOVE    WRK-SYS-TIME        TO  ORCSQRCSV62-SYSTIME
      *
           IF      ORCHC62-CSVKBN  NOT =   SPACE
      *        ＣＳＶ保存ファイル名 
               PERFORM 50034-QRCSVFILE-SEC
               MOVE    WRK-CSV-FILE        TO  ORCSQRCSV62-CSV-FILENAME
           END-IF
      *
           MOVE    ORCHC62-CSVKBN      TO  ORCSQRCSV62-CSV-KBN
           MOVE    WRK-QRKBN           TO  ORCSQRCSV62-QR-KBN
           MOVE    ORCHC62-CNT         TO  ORCSQRCSV62-CNT
           MOVE    ORCHC62-PGKBN       TO  ORCSQRCSV62-PGKBN
      *
           CALL    "ORCSQRCSV62"       USING
                                       SPA-AREA
                                       ORCSQRCSV62AREA
      *
      *    ＱＲイメージ印刷
           IF     (ORCSQRCSV62-RET-COUNT   >   ZERO)
             AND  (ORCSQRCSV62-RET-COUNT   <=  12  )
             AND  (WRK-QRKBN               =   "1"  )
             AND  (ORCHC62-PRTKBN          =   SPACE)
               PERFORM 5003-QRIMAGE-HEN-SEC
           END-IF
           .
       500-QR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *        ＱＲイメージ印刷処理
      *****************************************************************
       5003-QRIMAGE-HEN-SEC      SECTION.
      *
      *    ＱＲ空白画像位置
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  KUHAKU-FILE-NAME
      *
           MOVE    ZERO                TO  FLG-QROK
           MOVE    SPACE               TO  HCM62Q
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    ZERO                TO  WRK-FILE-CNT
           PERFORM VARYING     IDQ     FROM    1  BY  1
                   UNTIL       IDQ     >   12
               MOVE    KUHAKU-FILE-NAME    TO  HCM62Q-IMAGE-1 (IDQ)
               ADD     1                   TO  WRK-FILE-CNT
               IF      WRK-FILE-CNT        <=  ORCSQRCSV62-RET-COUNT 
                   MOVE    SPA-HOSPNUM         TO  QR-FILE-HOSPNUM
                   MOVE    CONS-FORM-ID        TO  QR-FILE-FORM-ID
                   MOVE    SPA-TERMID          TO  QR-FILE-TERMID
                   MOVE    WRK-SYS-TIME        TO  QR-FILE-SYOHMS
                   MOVE    WRK-FILE-CNT        TO  QR-FILE-ID
      *
      *v4.8
      *            一時ファイル名取得
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    QR-FILE-NAME-PARA   TO  SGETTEMP-BASENAME
                   CALL    "ORCSGETTEMP"       USING
                                                   SGETTEMP-AREA
                   MOVE    SGETTEMP-FULLNAME   TO  QR-FILE-NAME
      *
      *            ファイル存在チェック
                   INITIALIZE                      ORCSFILECHKAREA
                   MOVE    QR-FILE-NAME        TO  FILECHK-NAME
                   CALL    "ORCSFILECHK"       USING
                                               ORCSFILECHKAREA
                   IF      FILECHK-RC          =   ZERO OR 2
                       MOVE   QR-FILE-NAME     TO  HCM62Q-IMAGE-1 (IDQ)
                       MOVE   1                TO  FLG-QROK
                   END-IF
      *
                   IF      WRK-FILE-CNT        =   1
      *                帳票ＨＥＡＤセット処理
                       PERFORM 50010-HEAD-SET-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-QROK            =   1
      *        帳票出力処理
               PERFORM 50031-QRPRT-WRITE-SEC
           END-IF
           .
       5003-QRIMAGE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       50010-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE        =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           MOVE    SPACE           TO  HCM62Q-NAIYO2(CNT-LINE)
           STRING  WRK-ZZ9         DELIMITED  BY  SIZE
                   " 頁"           DELIMITED  BY  SIZE
                   INTO            HCM62Q-NAIYO2(CNT-LINE)
           END-STRING
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-2      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           MOVE    WRK-LINE-SEN    TO  HCM62Q-SEN(CNT-LINE)
      *
           MOVE    4               TO   CNT-LINE
           MOVE    WRK-LINE-SEN    TO  HCM62Q-SEN(CNT-LINE)
      *    医療機関名称セット
           MOVE    WRK-LINE-3      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-4      TO  HCM62Q-NAIYO1(CNT-LINE)
           .
       50010-HEAD-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       50031-QRPRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHC62-CNT     TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62Q          TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM62Q.red"    TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
     ***** MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(16)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT"          USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       50031-QRPRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータの変換　処理
      *****************************************************************
       50034-QRCSVFILE-SEC             SECTION.
      *
           IF      ORCHC62-DRCD        =   SPACE
               MOVE    "XXXXX"             TO  WRK-DRCD-E
           ELSE
               MOVE    ORCHC62-DRCD        TO  WRK-DRCD-E
           END-IF
      *
      *    ファイル名作成
           MOVE    SPACE               TO  WRK-CSV-FILE
           STRING  "okusuri_techo_"
                   ORCHC62-SRYYMD      DELIMITED   BY  SIZE
                   "_"
                   WRK-DRCD-E          DELIMITED   BY  SIZE
                   "_"
                   SPA-SYSYMD          DELIMITED   BY  SIZE
                   "_"
                   WRK-SYS-TIME        DELIMITED   BY  SIZE
                   ".csv"
                                       INTO    WRK-CSV-FILE
           END-STRING
      *
      *    ディレクトリ作成（医療機関識別番号）
      *    INITIALIZE                      ORCSMKDIRAREA
      *    MOVE    SPACE               TO  WRK-DIRECTORY-E
      *    STRING  WRK-DIRECTORY       DELIMITED   BY  SPACE
      *            "/"
      *            SPA-HOSPNUM         DELIMITED   BY  SIZE
      *                                INTO    WRK-DIRECTORY-E
      *    END-STRING
      *    MOVE    WRK-DIRECTORY-E     TO  MKDIR-DIRECTORY
      *    MOVE    "0755"              TO  MKDIR-MODE
      *    MOVE    ZERO                TO  MKDIR-RESULT
      *    CALL    "orcmkdir"          USING   ORCSMKDIRAREA
      *    IF      MKDIR-RESULT     NOT =  ZERO
      *        MOVE    "0"                 TO  PARA-CSVKBN
      *        GO  TO  50034-QRCSVFILE-EXT
      *    END-IF
      *
      *    ディレクトリ作成（診療日）
      *    INITIALIZE                      ORCSMKDIRAREA
      *    STRING  WRK-DIRECTORY-E     DELIMITED   BY  SPACE
      *            "/"
      *            SPA-SRYYMD          DELIMITED   BY  SIZE
      *                                INTO    MKDIR-DIRECTORY
      *    END-STRING
      *
      *    MOVE    MKDIR-DIRECTORY     TO  WRK-DIRECTORY-E
      *
      *    MOVE    "0755"              TO  MKDIR-MODE
      *    MOVE    ZERO                TO  MKDIR-RESULT
      *    CALL    "orcmkdir"          USING   ORCSMKDIRAREA
      *    IF      MKDIR-RESULT     NOT =  ZERO
      *        MOVE    "0"                 TO  PARA-CSVKBN
      *        GO  TO  50034-QRCSVFILE-EXT
      *    END-IF
      *    ファイル名作成
      *    MOVE    SPACE               TO  WRK-CSV-FILE
      *    STRING  WRK-DIRECTORY-E     DELIMITED   BY  SPACE
      *            "/JAHISTC"
      *            SPA-HOSPNUM         DELIMITED   BY  SIZE
      *            SPA-PTNUM           DELIMITED   BY  SPACE
      *            "-"
      *            "2"
      *            SPA-SRYYMD          DELIMITED   BY  SIZE
      *            WRK-DRCD-E          DELIMITED   BY  SIZE
      *            SPA-SYSYMD          DELIMITED   BY  SIZE
      *            WRK-SYS-TIME        DELIMITED   BY  SIZE
      *            ".csv"
      *                                INTO    WRK-CSV-FILE
      **** END-STRING
      *
           .
       50034-QRCSVFILE-EXT.
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
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
      *     MOVE    LNK-DAY2-EDTYMD3
      *                             TO  WRK-HENYMDG
      *     INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *--- 2015/11/18 START ------------------------------------------
      *****************************************************************
      *    中途終了  サブ処理
      *****************************************************************
       210-SUB-SEC                SECTION.
      *
      *    ワーク診療行為から医師情報を取得する
           PERFORM  900-WKSRYACT-SELECT-SEC
           PERFORM  UNTIL  FLG-WKSRYACT  =  1
      *
             INITIALIZE  WRK-DRCD
      *
             MOVE    WKSRY-DRCD             TO  WRK-DRCD
      *
      *      ドクターの編集
             IF    WKSRY-INPUTCD(1)(1:1)  =  "$"
               IF  WKSRY-INPUTCD(1)(5:4)  NUMERIC
                 MOVE    "1"                    TO  WRK-DRCD(1:1)
                 MOVE    WKSRY-INPUTCD(1)(5:4)  TO  WRK-DRCD(2:4)
               END-IF
             END-IF
      *
             MOVE    ZERO             TO  FLG-DRCD
             PERFORM  VARYING  IDX-ST  FROM  1  BY  1
                      UNTIL  ( IDX-ST     >  5
                      OR       FLG-DRCD  =  1 )
      *
               IF  H016-DRCD1(IDX-ST)  =  SPACE
                 MOVE    1                TO  FLG-DRCD
                 MOVE    WKSRY-DRCD       TO  H016-DRCD1(IDX-ST)
                 MOVE    WRK-DRCD         TO  H016-DRCD2(IDX-ST)
               ELSE
                 IF    ( WKSRY-DRCD  =  H016-DRCD1(IDX-ST)
                   AND   WRK-DRCD    =  H016-DRCD2(IDX-ST) )
                   MOVE    1                TO  FLG-DRCD
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             PERFORM  900-WKSRYACT-FETCH-SEC
           END-PERFORM
           PERFORM  900-WKSRYACT-CLOSE-SEC
      *
           .
       210-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確認  サブ処理
      *****************************************************************
       220-SUB-SEC                SECTION.
      *
           INITIALIZE  IDX-ST
           PERFORM  900-SYUNOU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-SYUNOU  =  1
      *
      *--- 2016/04/06 START ------------------------------------------
      **      該当データの更新時間（時分秒）へ15秒プラスする
      *       MOVE    SYU-UPHMS(1:6)      TO  TIME-SYUNOU-HMS(1:6)
      *       COMPUTE  TIME-SYUNOU-HMS  =  TIME-SYUNOU-HMS  +  15
      *      タイムスタンプを秒換算
             COMPUTE  TIME-STIME  =  (TIME-HH * 3600)
                                  +  (TIME-MM * 60)
                                  +  TIME-SS
      *      収納更新時間を秒換算（15秒プラスして）
             MOVE    SYU-UPHMS           TO  TIME-SYUNOU-HMS
             COMPUTE  TIME-SYUNOU-STIME  =  (TIME-SYU-HH * 3600)
                                         +  (TIME-SYU-MM * 60)
                                         +  TIME-SYU-SS
                                         +  15
      *--- 2016/04/06 END   ------------------------------------------
      *
      *
             IF     ( WRK-DENPNUM   NOT = SYU-DENPNUM )
               AND  ( TIME-YMD(3:6)     = SYU-UPYMD(3:6)
               AND  ( TIME-HMS(1:6)    >= SYU-UPHMS(1:6)
      *--- 2016/04/06 START ------------------------------------------
      *         AND    TIME-HMS(1:6)    <= TIME-SYUNOU-HMS(1:6) ) )
               AND    TIME-STIME       <= TIME-SYUNOU-STIME ) )
      *--- 2016/04/06 END   ------------------------------------------
      *
               ADD     1                   TO  IDX-ST
               MOVE    SYU-DENPNUM         TO  H016-DENPNUM(IDX-ST)
                                               WRK-DENPNUM
      *--- 2016/02/08 START ------------------------------------------
               MOVE    SYU-INGAISHOHOKBN
                                         TO  H016-INGAISHOHOKBN(IDX-ST)
      *--- 2016/02/08 END   ------------------------------------------
             END-IF
             PERFORM   900-SYUNOU-FETCH-SEC
           END-PERFORM
           PERFORM  900-SYUNOU-CLOSE-SEC
      *
           .
       220-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    印字グループチェック
      *****************************************************************
       220-BODY-GROUP-CHK-SEC         SECTION.
      *
           MOVE    ZERO              TO  FLG-GROUP-PRT
      *
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "31"
                                                  OR   "32"
                                                  OR   "33"
               IF  PARA-TYUSYA           =   "0"
                   MOVE    1         TO  FLG-GROUP-PRT
               END-IF
           END-IF
      *
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "60"
               IF  PARA-KENSA            =   "0"
                   MOVE    1         TO  FLG-GROUP-PRT
               END-IF
           END-IF
      *
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "70"
               IF  PARA-GAZOU            =   "0"
                   MOVE    1         TO  FLG-GROUP-PRT
               END-IF
           END-IF
      *
           .
       220-BODY-GROUP-CHK-EXT.
           EXIT.
      *
      *--- 2016/02/08 START ------------------------------------------
      *****************************************************************
      *    診療行為グループ名称
      *****************************************************************
      * 231-GROUPMEI-SYORI-SEC                   SECTION.
      *
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "21"
      *                                    OR   "22"
      *                                    OR   "23"
      *         IF  FLG-SRYKBN2         =    ZERO
      *             PERFORM 250-PAGE-CHK-SEC
      *             MOVE    "●内服・頓服・外用"
      *                                     TO  HCM62-NAIYO1(CNT-LINE)
      *             MOVE    1               TO  FLG-SRYKBN2
      *             MOVE    1               TO  FLG-GROUP
      *         END-IF
      **--- 2016/01/26 START ------------------------------------------
      **         IF      IDX-SRY         =   1
      **             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      **         END-IF
      **--- 2016/01/26 END   ------------------------------------------
      *     END-IF
      **
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "31"
      *                                    OR   "32"
      *                                    OR   "33"
      *         IF  FLG-SRYKBN3         =   ZERO
      *             PERFORM 250-PAGE-CHK-SEC
      *             MOVE    "●注射・点滴"
      *                                     TO  HCM62-NAIYO1(CNT-LINE)
      *             MOVE    1               TO  FLG-SRYKBN3
      *             MOVE    1               TO  FLG-GROUP
      *         END-IF
      **--- 2016/01/26 START ------------------------------------------
      **         IF      IDX-SRY         =   1
      **             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      **         END-IF
      **--- 2016/01/26 END   ------------------------------------------
      *     END-IF
      **
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "60"
      *         IF  FLG-SRYKBN6         =   ZERO
      *             PERFORM 250-PAGE-CHK-SEC
      *             MOVE    "●検査"
      *                                     TO  HCM62-NAIYO1(CNT-LINE)
      *             MOVE    1               TO  FLG-SRYKBN6
      *             MOVE    1               TO  FLG-GROUP
      *         END-IF
      **--- 2016/01/26 START ------------------------------------------
      **         IF      IDX-SRY         =   1
      **             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      **         END-IF
      **--- 2016/01/26 END   ------------------------------------------
      *     END-IF
      **
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "70"
      *         IF  FLG-SRYKBN7         =   ZERO
      *             PERFORM 250-PAGE-CHK-SEC
      **--- 2016/01/26 START ------------------------------------------
      **             MOVE    "●画像"
      *             MOVE    "●画像診断"
      **--- 2016/01/26 END   ------------------------------------------
      *                                     TO  HCM62-NAIYO1(CNT-LINE)
      *             MOVE    1               TO  FLG-SRYKBN7
      *             MOVE    1               TO  FLG-GROUP
      *         END-IF
      **--- 2016/01/26 START ------------------------------------------
      **         IF      IDX-SRY         =   1
      **             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      **         END-IF
      **--- 2016/01/26 END   ------------------------------------------
      *     END-IF
      **
      *     .
      * 231-GROUPMEI-SYORI-EXT.
      *     EXIT.
      *--- 2016/02/08 END   ------------------------------------------
      *
      *--- 2016/02/08 START ------------------------------------------
      *****************************************************************
      *    診療行為グループ名称２
      *****************************************************************
       231-GROUPMEI-SYORI2-SEC                   SECTION.
      *
           MOVE    SPACE                   TO  WRK-GROUPNAME
      *
           EVALUATE  ORCHC62-SRYKBN(IDX-ZAI)
               WHEN  "14"
                     MOVE  "●在宅"        TO  WRK-GROUPNAME
               WHEN  "21"
                     MOVE  "●内服"        TO  WRK-GROUPNAME
               WHEN  "22"
                     MOVE  "●頓服"        TO  WRK-GROUPNAME
               WHEN  "23"
                     MOVE  "●外用"        TO  WRK-GROUPNAME
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
                     MOVE  "●注射・点滴"  TO  WRK-GROUPNAME
               WHEN  "60"
                     MOVE  "●検査"        TO  WRK-GROUPNAME
               WHEN  "70"
                     MOVE  "●画像診断"    TO  WRK-GROUPNAME
           END-EVALUATE
      *
           IF  CNT-ZAI2                =  1
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-GROUPNAME
                                     TO  HCM62-NAIYO1(CNT-LINE)
               MOVE    1             TO  FLG-GROUP
           END-IF
      *
           .
       231-GROUPMEI-SYORI2-EXT.
           EXIT.
      *****************************************************************
      *    診療行為グループ名称３
      *****************************************************************
       231-GROUPMEI-SYORI3-SEC                   SECTION.
      *
           MOVE    SPACE                   TO  WRK-GROUPNAME
      *
           EVALUATE  ORCHC62-SRYKBN(IDX-ZAI)
               WHEN  "14"
                     MOVE  "●在宅"        TO  WRK-GROUPNAME
               WHEN  "21"
                     MOVE  "●内服"        TO  WRK-GROUPNAME
               WHEN  "22"
                     MOVE  "●頓服"        TO  WRK-GROUPNAME
               WHEN  "23"
                     MOVE  "●外用"        TO  WRK-GROUPNAME
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
                     MOVE  "●注射・点滴"  TO  WRK-GROUPNAME
               WHEN  "60"
                     MOVE  "●検査"        TO  WRK-GROUPNAME
               WHEN  "70"
                     MOVE  "●画像診断"    TO  WRK-GROUPNAME
           END-EVALUATE
      *
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-GROUPNAME
                                     TO  HCM62-NAIYO1(CNT-LINE)
               MOVE    1             TO  FLG-GROUP
      *
           .
       231-GROUPMEI-SYORI3-EXT.
           EXIT.
      *--- 2016/02/08 END   ------------------------------------------
      *
      *--- 2016/01/26 START ------------------------------------------
      *--- 2016/02/08 START ------------------------------------------
      *****************************************************************
      *    診療行為グループ連番
      *****************************************************************
      * 231-GROUP-REN-SEC                   SECTION.
      *
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "21"
      *                                     OR  "22"
      *                                     OR  "23"
      *        IF  CNT-SRY             =   1
      *             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      *         END-IF
      *     END-IF
      **
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "31"
      *                                     OR  "32"
      *                                     OR  "33"
      *         IF  CNT-SRY             =   1
      *             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      *         END-IF
      *     END-IF
      **
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "60"
      *         IF  CNT-SRY             =   1
      *             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      *         END-IF
      *     END-IF
      **
      *     IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "70"
      *         IF  CNT-SRY             =   1
      *             COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
      *         END-IF
      *     END-IF
      **
      *     .
      * 231-GROUP-REN-EXT.
      *     EXIT.
      **
      **--- 2016/01/26 END   ------------------------------------------
      *--- 2016/02/08 END   ------------------------------------------
      *
      *****************************************************************
      *    検査 画像の診療行為名称チェック
      *****************************************************************
       231-TENSUMEI-SYORI-SEC                   SECTION.
      *
      *--- 2016/03/02 START ------------------------------------------
      *     MOVE    ZERO                   TO  WRK-CHARFLG
      **
      **    検査
      *     IF    ( WRK-SRYKBN          =   "60"  )
      **
      *         IF    ( WRK-SRYCD(1:2)  NOT =   "16"  )
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         IF  TNS-DATAKBN   =   2
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         IF  TNS-KNSJISKBN   =   2
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      **        診療行為名に「〜加算」「〜判断料」などは印字しない
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "加算"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "減算"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "判断料"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "検査料"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "実施料"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      *     END-IF
      **
      **    画像
      *     IF    ( WRK-SRYKBN          =   "70"  )
      **
      *         IF    ( WRK-SRYCD(1:2)  NOT =   "17"  )
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         IF    ( TNS-SRYSYUKBN   =   "711"
      *                                OR   "732"  )
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         IF  TNS-DATAKBN   =   2
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         IF  TNS-KNSJISKBN   =   2
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      **        診療行為名に「〜加算」「〜判断料」などは印字しない
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "加算"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "減算"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "診断料"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *         MOVE    ZERO                   TO  WRK-CHARCNT
      *         INSPECT  TNS-NAME    TALLYING
      *                  WRK-CHARCNT  FOR  ALL  "手技料"
      *         IF  WRK-CHARCNT  >=  1
      *             MOVE    1                  TO  WRK-CHARFLG
      *             GO  TO  231-TENSUMEI-SYORI-EXT
      *         END-IF
      **
      *     END-IF
      *
           IF    ( WRK-SRYCD(1:2)  =   "16"  OR  "17" )
      *
               IF    ( WRK-SRYCD(1:2)  =   "17" )
      *         「〜剤注入」は印字しない
                 IF    ( TNS-SRYSYUKBN   =   "711"  OR   "732" )
                     MOVE    1                  TO  WRK-CHARFLG
                     GO  TO  231-TENSUMEI-SYORI-EXT
                 ELSE
                     MOVE    ZERO               TO  WRK-CHARFLG
                 END-IF
               END-IF
      *
      *        診療行為名に「〜加算」「〜判断料」とある場合は印字しない
               IF  TNS-DATAKBN   =   2
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               IF  TNS-KNSJISKBN   =   2
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "加算"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "減算"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "判断料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "検査料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "実施料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "診断料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "手技料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
           END-IF
      *--- 2016/03/02 END   ------------------------------------------
      *
           .
       231-TENSUMEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為  情報読込
      *****************************************************************
       900-WKSRYACT-SELECT-SEC          SECTION.
      *
           INITIALIZE                 WKSRYACT-REC
           MOVE    SPA-HOSPNUM        TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN      TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID           TO  WKSRY-PTID
           MOVE    SPA-SRYKA          TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD         TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM    TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_wksryact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-WKSRYACT-FETCH-SEC
           ELSE
             MOVE     1                 TO  FLG-WKSRYACT
           END-IF
      *
           .
       900-WKSRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為  情報検索
      *****************************************************************
       900-WKSRYACT-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_wksryact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-WKSRYACT
             MOVE    MCPDATA-REC        TO  WKSRYACT-REC
      *
           ELSE
             MOVE    1                  TO  FLG-WKSRYACT
           END-IF
      *
           .
       900-WKSRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為  クローズ処理
      *****************************************************************
       900-WKSRYACT-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_wksryact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-WKSRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報読み込み
      *****************************************************************
       900-SYUNOU-SELECT-SEC          SECTION.
      *
           INITIALIZE                 SYUNOU-REC
           MOVE    SPA-HOSPNUM        TO  SYU-HOSPNUM
           MOVE    "2"                TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID           TO  SYU-PTID
           MOVE    SPA-SRYYMD         TO  SYU-SRYYMD
           MOVE    SYUNOU-REC         TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    "key73"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-SYUNOU-FETCH-SEC
           ELSE
             MOVE     1                 TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    "key73"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-SYUNOU
             MOVE    MCPDATA-REC        TO  SYUNOU-REC
      *
      *      テスト患者のときは読み飛ばす
             INITIALIZE                     PTINF-REC
             MOVE    SYU-HOSPNUM        TO  PTINF-HOSPNUM
             MOVE    SYU-PTID           TO  PTINF-PTID
             MOVE    PTINF-REC          TO  MCPDATA-REC
             PERFORM 900-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
               AND  PTINF-TSTPTNUMKBN  =  "1"
               GO  TO  900-SYUNOU-FETCH-EXT
             END-IF
      *
           ELSE
             MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納クローズ処理
      *****************************************************************
       900-SYUNOU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    "key73"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYUNOU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *--- 2015/11/18 END   ------------------------------------------
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF    ( TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )  AND
                 ( FLG-TENSU           =   ZERO )
               PERFORM 900-TNSP-READ-SEC
           ELSE
               INITIALIZE                      TNSP-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読み込み
      *****************************************************************
       900-TNSP-READ-SEC           SECTION.
      *
           INITIALIZE                  TNSP-REC
           MOVE    WRK-SRYCD       TO  TNSP-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNSP-REC        TO  MCPDATA-REC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TNSP
                   MOVE    MCPDATA-REC     TO  TNSP-REC
               ELSE
                   MOVE    1               TO  FLG-TNSP
               END-IF
           ELSE
               MOVE    1               TO  FLG-TNSP
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
              AND (FLG-TNSP            =   ZERO  )
              AND (TNSP-USERTANICD NOT =   SPACE )
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
      *
           .
       900-TNSP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報読み込み
      *****************************************************************
       900-1010-READ-SEC           SECTION.
      *
      *    職員情報読み込み
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    ORCHC62-DRCD    TO  SYS-1010-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           MOVE    SYS-1010-NAME   TO  WRK-DRNAME
      *
           .
       900-1010-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1031-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    ORCHC62-SRYYMD  TO  ORCSPRTNM-SRYYMD
           MOVE    "2"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-1031-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(18) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1901-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1902-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1902-REC
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *----(04.08.02)--ADD-START---
      *****************************************************************
      *    薬情マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC        SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    TNS-SRYCD       TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
               END-IF
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *----(04.08.02)--ADD-END-----
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
