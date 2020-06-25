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
       PROGRAM-ID.             ORCR0460.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      点数摘要欄編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.08.23  検査にコメントがあるとき検査数
      *                                     にカウントしないようにする
      *  01.00.02    NACL-藤原    02.08.26  皮下筋肉注射以外は手技名を印刷
      *                                     するようにする
      *  01.00.03    NACL-門脇    02.09.19  自賠責対応
      *  01.00.04    NACL-藤原    02.09.20  検査のときの表示方法を変更
      *  01.00.05    NACL-門脇    02.10.02  摘要出力項目変更
      *  01.00.06    NACL-門脇    02.11.26  診療実日数算定用の
      *                                     コードを印刷しないよう追加 
      *                                     検診コードを印刷するよう追加
      *  01.00.07    NACL-藤原    02.12.02  酸素の時単価を表示するように追加
      *  01.00.08    NACL-藤原    03.01.10  検査の回数の編集
      *                                     画像診断のフィルムを手技に表示
      *  01.00.09    NACL-門脇    03.02.13  乳幼児加算の時手技に表示
      *  01.00.10    NACL-門脇    03.03.06  初再診のコメント編集対応
      *  01.00.11    NACL-門脇    03.03.12  先頭の診療行為コードの判定修正
      *  01.00.12    NACL-門脇    03.05.08  先頭の診療行為コードの判定修正
      *  01.00.13    NACL-門脇    03.05.13  腰部固定帯加算を器材点へ
      *  01.00.14    NACL-藤原    03.05.27  検査のまとめの修正
      *  01.00.15    NACL-門脇    03.06.05  摘要欄出力条件修正
      *  01.00.16    NACL-藤原    03.06.30  薬剤・器材の剤点数が０のとき
      *                                     摘要欄に印刷しないように修正
      *  01.00.17    NACL-門脇    03.09.29  点数マスタを読み込まないよう
      *                                     修正
      *  01.00.18    NACL-藤原    04.02.05  薬剤・器材の編集順を逆にする
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.01  初診（乳幼児）（夜間・休日・
      *                                     深夜）加算の摘要欄記載
      *  01.01.02    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.03    NACL-門脇    04.04.26  ユーザーコメント追加対応
      *  01.01.04    NACL-門脇    04.06.11  皮下筋肉注射で、薬剤点・器材点
      *                                     が無いの場合の摘要欄記載対応
      *  01.01.05    NACL-藤原    04.07.16  検査のまとめを回数テーブルの
      *                                     合計で行うようにする 
      *  01.01.06    NACL-門脇    04.09.22  薬剤・器材の記載順修正
      *  中止処理追加
      *  01.02.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.02.02    NACL-門脇    04/12/17  胸部固定帯加算、創外固定器
      *                                     使用加算を薬剤等へ
      *  01.02.03    NACL-門脇    05/01/27  腰・胸部固定帯加算、創外固定器
      *                                     使用加算の技術・薬剤等の選択対応
      *  01.02.04    NACL-門脇    05/02/28  （サ）記載対応
      *  01.02.05    NACL-門脇    05/03/02  薬剤で単位のないデータの数量編集
      *                                     修正
      *  01.02.06    NACL-藤原    05/03/07  包括対象検査のまとめの修正
      *  01.02.07    NACL-藤原    05/04/18  包括対象検査がまとまらない
      *                                     場合の編集の修正
      *  01.02.08    NACL-門脇    05/05/26  手術・麻酔の記載順変更対応
      *  01.02.09    NACL-門脇    05/06/06  ワーク項目桁数修正
      *  01.02.10    NACL-藤原    05/08/17  包括対象検査フラグの新設に対応
      *  01.02.11    NACL-門脇    05/08/18  自賠責従来様式対応
      *  01.02.12    NACL-伊藤    05/09/01  検体検査コメントのついた包括
      *                                     対象検査の剤まとめに対応
      *  01.02.13    NACL-門脇    05/11/01  （検選）等コメント対応
      *
      *  ２００６年４月改正対応
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  02.09.02    NACL-藤原    06/04/14  包括対象検査「１１」の追加
      *  02.09.03    NACL-門脇    06/05/22  電子化加算編集修正
      *  03.01.01    NACL-門脇    06/06/28  実日数減算コード対応
      *  03.01.02    NACL-門脇    06/07/10  在支援コメント自動記載対応
      *  03.01.03    NACL-門脇    06/07/14  診療区分置き換えフラグの編集
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.04.02    NACL-門脇    07/03/23  月途中医療機関コード変更時対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/09/13  点滴の記載順変更（日付順）
      *  03.05.04    NACL-門脇    07/09/13  皮下筋肉、静脈注射の記載変更対応
      *
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.01.02    NACL-門脇    07/11/26  特定器材商品名記載対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （初診（夜間・早朝等）加算追加）
      *                                     （診療区分６４：病理診断対応）
      *                                     （再就労、職業復帰：記載順対応）
      *  04.02.02    NACL-門脇    08/04/21  ８０その他記載順対応
      *  04.02.03    NACL-門脇    08/06/25  ８０その他記載順対応
      *  04.02.04    NACL-門脇    08/11/06  皮下筋肉、静脈注射の記載変更対応
      *
      *  04.03.01    NACL-門脇    08/07/09  投薬の記載順変更（日付順）
      *  04.03.02    NACL-門脇    08/07/14  投薬剤点０（院外等）記載対応
      *  04.03.03    NACL-門脇    08/12/01  在宅（院外分）編集修正
      *  04.03.04    NACL-門脇    09/01/15  処方（乳幼児）加算編集対応
      *                                     処方せん（乳幼児）加算編集対応
      *
      *  04.04.01    NACL-門脇    09/02/04  検査のコメント自動記載対応
      *  04.04.02    NACL-門脇    09/09/30  検査の数量編集修正
      *  04.04.03    NACL-門脇    09/11/19  自賠責健保準拠療養担当手当対応
      *  04.04.04    NACL-門脇    10/03/25  改正対応
      *                                     （地域医療貢献加算記載編集）
      *                                     （明細書発行体制等加算記載編集）
      *  04.04.05    NACL-門脇    10/03/29  改正対応
      *                                     （画像診断２回目以降減算編集）
      *                                     （コード追加）
      *  04.04.06    NACL-門脇    10/03/31  改正対応
      *                                     （頸部固定帯加算の対応）
      *                                     （８０その他の記載順）
      *  04.04.07    NACL-門脇    10/09/03  在宅薬剤院外処方点検用レセ
      *                                     記載修正
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    09/06/17  処置の記載順変更（日付順）
      *  04.05.03    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.04    NACL-門脇    10/01/21  治験対応
      *  04.05.05    NACL-門脇    10/01/26  東京都マル子追加対応
      *  04.05.06    NACL-門脇    11/02/28  長崎県地方公費対応
      *  04.05.07    NACL-門脇    11/11/30  画像診断フイルム分画数対応
      *  04.05.08    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     「再診」の対応
      *                                     「その他」の対応
      *  04.05.09    NACL-門脇    13/01/22  ワーク領域拡張
      *
      *  04.06.01    NACL-門脇    12/10/15  診療行為入力したコメントのみの剤の
      *                                     レセプト記載対応
      *  04.06.02    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *  04.06.03    NACL-門脇    13/02/27  平成２５年４月改正対応
      *  04.06.04    NACL-門脇    13/11/07  公務災害（労災準拠）の地公災特別
      *                                     加算対応（京都府）
      *
      *  04.07.01    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.02    NACL-門脇    13/10/02  自賠責（第三者行為）対応
      *  04.07.03    NACL-門脇    13/12/03  自賠責（労災準拠）のレセプト様式
      *                                     追加対応
      *  04.07.04    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.05    NACL-門脇    14/09/01  平成２６年１０月改正対応
      *  04.07.06    NACL-門脇    14/09/11  摘要欄に印字しない診療行為コードの
      *                                     データ区分２、かつ、診療区分が点数
      *                                     マスタの診療区分と異なる場合の対応
      *  04.07.07    NACL-門脇    14/11/12  画像診断フイルム分画数対応修正
      *  04.07.08    NACL-門脇    14/12/15  平成２７年１月改正対応
      *                                     （初再診料・低妥結率対応）
      *  04.07.09    NACL-門脇    14/12/24  平成２７年１月改正対応
      *                                     （初再診料・低妥結率対応）
      *  04.07.10    NACL-門脇    15/02/05  摘要欄の点数・回数等の記載
      *                                     不具合対応
      *  04.07.11    NACL-門脇    15/03/09  訪問診療にかかる記録書コメント対応
      *  04.07.12    NACL-門脇    15/03/16  平成２７年４月改正対応
      *
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.03    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.04    NACL-門脇    15/12/02  ０点地域公費記載対応
      *  04.08.05    NACL-門脇    15/12/08  労災コードの表用番号・公表順序番号
      *                                     編集修正
      *  04.08.06    NACL-門脇    16/01/12  点検用レセプト院外処方一般名記載対応
      *  04.08.07    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.08    NACL-門脇    16/09/20  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.09    NACL-門脇    16/11/07  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応修正
      *  04.08.10    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      *  04.08.11    NACL-門脇    17/12/13  包括点数の公害補償法減点分のマイナス
      *                                     表示対応
      *  04.08.12    NACL-藤原    18/03/28  平成３０年４月改正対応
      *  04.08.13    NACL-門脇    18/03/29  平成３０年４月改正対応
      *
      *  05.00.01    NACL-門脇    18/11/16  宮城県地方公費対応
      *  05.00.02    NACL-門脇    19/03/11  公務災害（労災準拠）の地公災特別
      *                                     加算対応（京都府）修正
      *  05.00.03    NACL-門脇    19/03/11  予約コード読み飛ばし修正
      *  05.00.04    NACL-門脇    20/03/30  令和２年４月改正対応
      *  05.00.05    NACL-藤原    20/04/01  令和２年４月改正対応
      *                                     包括対象検査の追加
      *                                     名称の桁数変更による記載対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *   
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
      *    
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
      *   
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *    
           SELECT  RECE46X-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46X-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF044.INC".
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
       FD  RECE46-FILE.
       01  RECE46-REC. 
           COPY    "CPRCF046.INC".
      *
       FD  RECE46X-FILE.
       01  RECE46X-REC. 
           COPY    "CPRCF046.INC"  REPLACING  //RECE46//
                                   BY         //RECE46X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE01//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE46//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  SPA-STS             PIC 9(04).
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE46          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE41          PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-NOPRT           PIC 9(01).
           03  FLG-KENSA           PIC 9(01).
           03  FLG-TEIGEN          PIC 9(01).
           03  FLG-COMMENT         PIC 9(01).
           03  FLG-COMMENT2        PIC 9(01).
           03  FLG-COMMENT3        PIC 9(01).
           03  FLG-HIKAKIN         PIC 9(01).
           03  FLG-SRYCD1          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-GAIJINKEN       PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-RECE46          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
           03  CNT-SRYKBN60        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDV                 PIC 9(04). 
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *    点数回数編集用     
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
      *    剤診療行為コード別編集用     
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(03).
           03  IDX9                PIC 9(03).
      *
           03  WRK-SYU-INDEX       PIC 9(04).
           03  WRK-YKZ-INDEX       PIC 9(04).
           03  WRK-KIZ-INDEX       PIC 9(04).
      *
           03  IDW3                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-RECEKBN    PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN    PIC X(01).
           03  WRK-PARA-PRTKBN         PIC X(01).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-LINENUM         PIC 9(05).
      *
           03  WRK-TENSUGOKEI      PIC 9(07).
      *    剤印字行数
           03  WRK-TBLMAX          PIC 9(03).
      *    剤明細総数（診療行為コード別）
           03  WRK-ZAIWMAX-G.
               05  WRK-ZAIWMAX     PIC 9(03)   OCCURS  6.
           03  WRK-ZAIWMAX-SAV-G.
               05  WRK-ZAIWMAX-SAV PIC 9(03)   OCCURS  6.
      * 
           03  WRK-YUKOKETA        PIC  9(03).
           03  WRK-NAME            PIC  X(300).
      *
           03  WRK-RECE45          PIC 9(01).     
      *
      *    数字半角全角変換用     
           03  WRK-NUM                         PIC 9(01).
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(10).
               05  WRK-CHAR-N                  PIC X(20).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-FLG                PIC 9(01). 
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *
           03  WRK-EDIT-AREA.
               05  WRK-CLM                     PIC 9(03).
               05  WRK-WCLM                    PIC 9(03).
      *
           03  WRK-KOUI                        PIC X(300).
      *
           03  WRK-ZAI.
               05  WRK-ZAI-MK                  PIC X(02).
               05  WRK-ZAI-NAME.
                   07  FILLER                  PIC X(28).
                   07  WRK-ZAI-TNSKAI          PIC X(22).
      *
           03  WRK-TNSKAI.
               05  WRK-ZAITEN                  PIC X(14).
               05  WRK-X                       PIC X(02).
               05  WRK-ZAIKAISU                PIC X(06).
      *
           03  WRK-ZAITENX                     PIC ZZZZZZ9.
           03  WRK-ZAIKAISUX                   PIC ZZ9.
           03  WRK-TNSKAI-ST                   PIC 9(02).
           03  WRK-TNSKAI-KETA                 PIC 9(02).
      *
           03  WRK-SRYCD                       PIC X(01).
      *     
           03  WRK-R05IDX                      PIC 9(02).
      *
      *    検査の回数
           03  WRK-KENSA-KAISU                 PIC 9(05). 
      *     
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-SRYSURYOW                   PIC X(24).
           03  WRK-ZAIIDX                      PIC 9(03).
           03  WRK-NAMEKETA                    PIC 9(03).
           03  WRK-YOHAKU                      PIC 9(03).
           03  WRK-WKETA                       PIC 9(03).
      *    分画数編集用     
           03  WRK-BUNKAKUKETA                 PIC 9(02).
           03  WRK-BUNKAKU-N                   PIC X(10).
           03  WRK-SRYKAISU-X                  PIC ZZ9.
      *    数量編集用     
           03  WRK-SURYOKETA                   PIC 9(02).
           03  WRK-SURYOKETA1                  PIC 9(02).
           03  WRK-SURYOKETA2                  PIC 9(02).
           03  WRK-SURYOST                     PIC 9(02).
           03  WRK-SURYO                       PIC 9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1                PIC 9(05).
               05  WRK-SURYO-S2                PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                PIC ZZZZZ.
               05  WRK-SURYO-Z2                PIC X(01).
               05  WRK-SURYO-Z3                PIC ZZZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1                PIC X(10).
               05  WRK-SURYO-N2                PIC X(02).
               05  WRK-SURYO-N3                PIC X(10).
      *
      *    特定保険医療材料編集用
           03  WRK-RECEHENNAME                 PIC X(300).
           03  WRK-RECEHENTANINAME             PIC X(50).
           03  WRK-RECEHENTANI                 PIC 9(02).
      *
      *    剤診療行為コード別編集用     
           03  WRK-WIDX1                       PIC 9(03).
           03  WRK-WIDX2                       PIC 9(03).
      *     
           03  WRK-Z9                          PIC ZZZ9.
      *
           03  WRK-SRYSYUKBN                   PIC X(03).
      *
           03  WRK-KENTAICOMMENT-YUKOKETA      PIC 9(03).
           03  WRK-KENTAICOMMENT-NAME          PIC X(300).
           03  WRK-KENTAICOMMENT-NAME-TEMP     PIC X(300).
           03  WRK-KENTAICOMMENT-SRYCD         PIC 9(09).
      *
           03  WRK-SET-LINENUM                 PIC 9(05).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    剤・診療行為テーブル
       01  TBL-ZAI-AREA.
           03  TBL-ZAI      OCCURS 100  PIC X(52).
      *
      *    検査をまとめるテーブル
       01  WRK-KENSA-MATOME-MAX            PIC 9(03).
       01  WRK-KENSA-MATOME-TABLE.
           03  WRK-KENSA-MATOME            OCCURS  101.
               05  WRK-HOUKNSKBN           PIC 9(02).
               05  WRK-GAIJINKEN           PIC 9(02).
               05  WRK-SRYSYUKBN-K         PIC X(03).
               05  WRK-KAISU-G.
                   07  WRK-KAKSU-OCC       OCCURS  10.
                       09  WRK-KAISU       PIC 9(03).
                   07  WRK-KAISU1          PIC 9(02).
                   07  WRK-KAISU2          PIC 9(03).
               05  WRK-KENSA-SU            PIC 9(03).
               05  WRK-RECE44-IDX-G.
                   07  WRK-RECE44-IDX      PIC 9(03)
                                                  OCCURS  30.
               05  WRK-KENSA-SRYCD-G.
                   07  WRK-KENSA-SRYCD-OCC OCCURS  30.
                       09  WRK-KENSA-KENTAICOMMENT-SRYCD
                                           PIC 9(09).
                       09  WRK-KENSA-SRYCD PIC 9(09).
                       09  WRK-KENSA-SRYSURYO PIC 9(05)V9(05).
      *
       01  WRK-RECE44-ZAITEN               PIC 9(08). 
      *         
       01  WRK-RECE44-MAX                  PIC 9(03).
       01  WRK-SUM-KAISU-TABLE.
           03  WRK-SUM-KAISU               PIC 9(04)
                                                  OCCURS  31.
       01  WRK-MIN-KAISU-TABLE.
           03  WRK-MIN-KAISU-G             OCCURS  31.
               05  WRK-MIN-KAISU           PIC 9(03).
               05  WRK-MIN-HOUKNSKBN       PIC 9(02).
               05  WRK-MIN-GAIJINKEN       PIC 9(02).
               05  WRK-MIN-SRYSYUKBN       PIC X(03).
       01  WRK-RECE44-FLG-TABLE.
           03  WRK-RECE44-FLG              PIC 9(01)
                                                  OCCURS  100.
       01  WRK-RECE44-MATOME-TABLE.
           03  WRK-RECE44-MATOME           PIC 9(01)
                                                  OCCURS  100.
       01  WRK-RECE44-TABLE.
           03  WRK-RECE44-TBL              OCCURS  100.
               05  WRK-RECE44-HOUKNSKBN           PIC 9(02).
               05  WRK-RECE44-GAIJINKEN           PIC 9(02).
               05  WRK-RECE44-SRYSYUKBN           PIC X(03).
               05  WRK-RECE44-REC                 PIC X(2000).
               05  WRK-RECE44-SRYCD-G.
                   07  WRK-RECE44-KENTAICOMMENT-SRYCD
                                                  PIC 9(09).
                   07  WRK-RECE44-SRYCD           PIC 9(09).
                   07  WRK-RECE44-SRYSURYO        PIC 9(05)V9(05).
               05  WRK-RECE44-DAY-AREA.
                   07  WRK-RECE44-DAY-TBL         OCCURS  10.
                       09  WRK-RECE44-KAISUINF    OCCURS  31.
                           11  WRK-RECE44-KAISU   PIC  9(03).
      *
      *    検査のとき適用の編集に使用
       01  WRK-KENSA-MAX                   PIC 9(04).
       01  TBL-NAME-AREA.
           03  TBL-NAME-REC      OCCURS 100.
               05  TBL-SRYCD               PIC  X(09).
               05  TBL-SRYSURYO            PIC  9(05)V9(05).
      *        単位コード/単位名称有効桁数/単位名称
               05  TBL-TANICD              PIC  9(03).
               05  TBL-TANIMOJISU          PIC  9(01).
               05  TBL-TANINAME            PIC  X(24).
               05  TBL-NAMEINF.
      *            漢字有効桁数/漢字名称
                   07  TBL-YUKOKETA        PIC  9(03).
                   07  TBL-NAME            PIC  X(300).
      *        データ区分情報
               05  TBL-DATAKBN             PIC  9(01).
      *        包括対象検査情報
               05  TBL-HOUKNSKBN           PIC  9(02).
      *
      *     検査の項目数用
        01  WRK-KOUMOKUSU               PIC 9(02). 
        01  WRK-TNS-HOUKNSKBN           PIC 9(02).
        01  WRK-CONS-KENSACOMMENT-AREA.
            03  WRK-CONS-KENSACOMMENT-G.
                05  FILLER                 PIC X(50)   VALUE
                "血液化学検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "内分泌学的検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "肝炎ウイルス関連検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "腫瘍マーカーのうちα−フェトプロテイン等".                        
                05  FILLER                 PIC X(50)   VALUE
                "腫瘍マーカー".                        
                05  FILLER                 PIC X(50)   VALUE
                "出血・凝固検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "自己抗体検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "内分泌負荷試験".                        
                05  FILLER                 PIC X(50)   VALUE
                "ウイルス抗体価".                        
                05  FILLER                 PIC X(50)   VALUE
                "ウイルス抗体価精密測定".                        
                05  FILLER                 PIC X(50)   VALUE
                "血漿蛋白免疫学的検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "悪性腫瘍遺伝子検査".
                05  FILLER                 PIC X(50)   VALUE
                "悪性腫瘍遺伝子検査（処理が容易なもの）".
                05  FILLER                 PIC X(50)   VALUE
                "悪性腫瘍遺伝子検査（処理が複雑なもの）".
            03  WRK-CONS-KENSACOMMENT-R    REDEFINES
                                           WRK-CONS-KENSACOMMENT-G.
                05  WRK-CONS-KENSACOMMENT  PIC X(50)   OCCURS  14.
      *外来迅速検体検査加算算定日テーブル
       01  WRK-KENTAIKASAN-TABLE.
           03  WRK-KENTAIKASAN             PIC 9(01)
                                                  OCCURS  31.
       01  WRK-GAIJINKEN-ZAINUM            PIC 9(03).
      *外来迅速検体検査加算算定日の剤情報退避
       01  WRK-RECE44GJK-MAX               PIC 9(02).
       01  WRK-RECE44GJK-TABLE.
           03  WRK-RECE44GJK-TBL           OCCURS 31.
               05  WRK-RECE44GJK-REC       PIC X(2000).
       01  RECE44GJK-REC.
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44GJK//.
       01  RECE44SAV-REC.
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44SAV//.
      *          
       01  TBL-ZAIW-AREA.
      *    剤の先頭の診療コード
           03  WRK-ZAIW-SRYCD1             PIC X(09).
      *    剤の先頭の診療コードの各種番号等情報
           03  WRK-ZAIW-CDKBN-AREA.
               05  WRK-ZAIW-CDKBN-KBN          PIC X(01).
               05  WRK-ZAIW-CDKBN-SYO          PIC 9(01).
               05  WRK-ZAIW-CDKBN-BU           PIC 9(02).
               05  WRK-ZAIW-CDKBN-KBNNUM       PIC 9(03).
               05  WRK-ZAIW-CDKBN-KBNNUM-EDA   PIC 9(02).
               05  WRK-ZAIW-CDKBN-KOUBAN       PIC 9(02).
               05  WRK-ZAIW-KOUHYOJYUNNUM      PIC 9(09).
      *    剤の先頭の診療コードの検査等実施判断情報
           03  WRK-ZAIW-KNSJISKBN          PIC 9(01).
      *     
      *    診療行為別テーブル
      *    １：ユーザーコメント、２：コメント、
      *    ３：診療行為、４：薬剤、５：特定器材、６：外迅検退避用
           03  TBL-ZAIW-OCC                OCCURS 6.
               05  TBL-ZAIW-REC            OCCURS 100.
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード/数量/回数
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
                       09  TBL-ZAIW-SRYSYUKBN-X     PIC  X(03).
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード/単位名称有効桁数/単位名称
                       09  TBL-ZAIW-TANICD          PIC  9(03).
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分/点数/明細請求フラグ
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(02).
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分/名称入力番号/点数欄集計先識別
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *
                       09  TBL-ZAIW-DATAKBN         PIC  9(01).
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
                       09  TBL-ZAIW-KNSJISKBN       PIC  9(01).
                       09  TBL-ZAIW-KNSJISGRPKBN    PIC  9(02).
                       09  TBL-ZAIW-CDKBNINF.
                         11 TBL-ZAIW-CDKBN-KBN        PIC  X(01).
                         11 TBL-ZAIW-CDKBN-SYO        PIC  9(01).
                         11 TBL-ZAIW-CDKBN-BU         PIC  9(02).
                         11 TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                         11 TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                         11 TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                         11 TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
                       09  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
                       09  TBL-ZAIW-KOKUJISKBKBN2   PIC  9(01).
                       09  TBL-ZAIW-TUSOKUGAIKBN    PIC  9(01).
                       09  TBL-ZAIW-TIMEKSNKBN      PIC  9(01).
                       09  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
                       09  TBL-ZAIW-CHUKSNINF.
                         11 TBL-ZAIW-CHUKSNCD       PIC  X(04).
                         11 TBL-ZAIW-CHUKSNTUBAN    PIC  X(01).
                       09  TBL-ZAIW-KENTAICOMMENT   PIC  9(01).
                       09  TBL-ZAIW-INPUTCOMENT     PIC  X(140).
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数/漢字名称
                       09  TBL-ZAIW-YUKOKETA        PIC  9(03).
                       09  TBL-ZAIW-NAME            PIC  X(300).
                   07  TBL-ZAIW-SRYSYUKBN-INPUT     PIC  X(03).
                   07  TBL-ZAIW-KENTAICOMMENT-SRYCD PIC  9(09).
      *            レセ用特定器材商品名編集フラグ
                   07  TBL-ZAIW-RECEHENFLG          PIC  X(01).
      *            点数マスタの診療区分
                   07  TBL-ZAIW-TNSSRYKBN           PIC  X(02).
      *
       01  TBL-ZAI-MAX         PIC  9(03)  VALUE   100.
      *
      *    初診・再診の剤まとめ情報
       01  TBL-SORT11-INF.
         02    TBL-SORT11-OCC              OCCURS   4.
           03  TBL-SORT11-FLG              PIC  9(01).
           03  TBL-SORT11-TEN              PIC  9(07).
           03  TBL-SORT11-YUKOKETA         PIC  9(03).
           03  TBL-SORT11-NAME             PIC  X(300).
           03  TBL-SORT11-ZAIKAISU         PIC  9(04).
           03  TBL-SORT11-HKNCOMBI         PIC  9(04).
      *
       01  TBL-SORT12-INF.
         02    TBL-SORT12-OCC              OCCURS  32.
           03  TBL-SORT12-FLG              PIC  9(01).
           03  TBL-SORT12-TEN              PIC  9(07).
           03  TBL-SORT12-YUKOKETA         PIC  9(03).
           03  TBL-SORT12-NAME             PIC  X(300).
           03  TBL-SORT12-ZAIKAISU         PIC  9(04).
           03  TBL-SORT12-HKNCOMBI         PIC  9(04).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *   印字しない診療行為テーブル
       01  TBL-NOSRYCD-AREA.
      *    初診（病院）
           03  FILLER          PIC X(09)   VALUE   "111000110".
      *    初診（診療所）
           03  FILLER          PIC X(09)   VALUE   "111003610".
      *    初診（病院）
           03  FILLER          PIC X(09)   VALUE   "111700110".
      *    初診（診療所）
           03  FILLER          PIC X(09)   VALUE   "111700210".
      *    再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112009210".
      *    同日再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112010150".
      *    電話等再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112009750".
      *    同日電話等再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112010650".
      *    老人再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112703710".
      *    老人同日再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112704450".
      *    老人電話等再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112704150".
      *    老人同日電話等再診（１回目）
           03  FILLER          PIC X(09)   VALUE   "112704850".
      *    外来管理加算（３回目まで）
           03  FILLER          PIC X(09)   VALUE   "112011010".
      *    老人外来管理加算（３回目まで）
           03  FILLER          PIC X(09)   VALUE   "112705310".
      *    老人外来管理加算（病院）（３回目まで）
           03  FILLER          PIC X(09)   VALUE   "112705110".
      *    再診（乳幼児）加算
      *****03  FILLER          PIC X(09)   VALUE   "112000970".
      *    再診（乳幼児）（時間外等）加算
           03  FILLER          PIC X(09)   VALUE   "112006970".
      *    再診（幼児）加算
           03  FILLER          PIC X(09)   VALUE   "112001070".
      *    再診（幼児）（時間外等）加算
           03  FILLER          PIC X(09)   VALUE   "112007070".
      *    時間外（再診）
           03  FILLER          PIC X(09)   VALUE   "112001110".
      *    時間外（再診）
           03  FILLER          PIC X(09)   VALUE   "112701010".
      *    休診（再診）
           03  FILLER          PIC X(09)   VALUE   "112001210".
      *    休日（再診）
           03  FILLER          PIC X(09)   VALUE   "112701110".
      *    深夜（再診）
           03  FILLER          PIC X(09)   VALUE   "112001310".
      *    深夜（再診）
           03  FILLER          PIC X(09)   VALUE   "112701210".
      *    処方料（７種類以上）
           03  FILLER          PIC X(09)   VALUE   "120002610".
      *    処方料（その他）
           03  FILLER          PIC X(09)   VALUE   "120001210".
      *    処方料（麻・向・覚・毒）
           03  FILLER          PIC X(09)   VALUE   "120001310".
      *    調基（その他）
           03  FILLER          PIC X(09)   VALUE   "120001810".
      *    調基（入院）
           03  FILLER          PIC X(09)   VALUE   "120001710".
      *    調剤・処方料（麻・向・覚・毒）
           03  FILLER          PIC X(09)   VALUE   "120002030".
      *    調剤料（外用薬）
           03  FILLER          PIC X(09)   VALUE   "120001010".
      *    調剤料（内用薬・浸煎薬・頓服薬）
           03  FILLER          PIC X(09)   VALUE   "120000710".
      *    調剤料（入院）
           03  FILLER          PIC X(09)   VALUE   "120001110".
      *    調剤料（麻・向・覚・毒）（入院）
           03  FILLER          PIC X(09)   VALUE   "120000410".
      *    調剤料（麻・向・覚・毒）（入院外）
           03  FILLER          PIC X(09)   VALUE   "120000110".
      *    初診料算定科：
           03  FILLER          PIC X(09)   VALUE   "830000020".
      *    再診料算定科：
           03  FILLER          PIC X(09)   VALUE   "830000021".
      *    再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112007410".
      *    老人再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112702310".
      *    電話等再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112007950".
      *    老人電話等再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112702750".
      *    同日電話等再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112008850".
      *    老人同日電話等再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112703450".
      *    同日再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112008350".
      *    老人同日再診（病院）(１回目）
           03  FILLER          PIC X(09)   VALUE   "112703050".
      *    再診（乳幼児時間外）加算
           03  FILLER          PIC X(09)   VALUE   "112014770".
      *    再診（乳幼児休日）加算
           03  FILLER          PIC X(09)   VALUE   "112014870".
      *    再診（乳幼児深夜）加算
           03  FILLER          PIC X(09)   VALUE   "112014970".
      *    処方（乳幼児）加算
           03  FILLER          PIC X(09)   VALUE   "120002170".
      *    処方料（向精神薬多剤投与）
           03  FILLER          PIC X(09)   VALUE   "120003610".
      *    処方料（紹介率が低い大病院３０日以上投薬減算）
           03  FILLER          PIC X(09)   VALUE   "120003870".
      *    処方料（向精神薬長期処方）
           03  FILLER          PIC X(09)   VALUE   "120004410".
      *
      *    実日数算定
           03  FILLER          PIC X(09)   VALUE   "099409901".
      *    初診料ダミー
           03  FILLER          PIC X(09)   VALUE   "099110001".
      *    再診料ダミー
           03  FILLER          PIC X(09)   VALUE   "099120001".
      *    実日数減算
           03  FILLER          PIC X(09)   VALUE   "099409903".
      *
      *    外来管理加算（３月以内）
           03  FILLER          PIC X(09)   VALUE   "101120070".
      *    外来管理加算（３月以内）
           03  FILLER          PIC X(09)   VALUE   "101120020".
      *    外来管理加算（３月超）（５回以内）
           03  FILLER          PIC X(09)   VALUE   "101120080".
      *
      *******
      *
      *    電話等再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023350".
      *    同日電話等再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023450".
      *    再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016610".
      *    電話等再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016750".
      *    電話等特定妥結率再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023650".
      *    同日再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016850".
      *    同日電話等再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016950".
      *    同日電話等特定妥結率再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023750".
      *
      *    再診（乳幼児）加算
           03  FILLER          PIC X(09)   VALUE   "112000970".
      *
      *    妊婦時間外加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021370".
      *    妊婦休日加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021470".
      *    妊婦深夜加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021570".
      *    時間外特例（再診）
           03  FILLER          PIC X(09)   VALUE   "112001410".
      *    再診（乳幼児時間外特例医療機関）加算
           03  FILLER          PIC X(09)   VALUE   "112015070".
      *    妊婦時間外特例医療機関加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021670".
      *    小児科再診（乳幼児夜間）加算（６歳未満）
           03  FILLER          PIC X(09)   VALUE   "112014170".
      *    小児科再診（乳幼児休日）加算（６歳未満）
           03  FILLER          PIC X(09)   VALUE   "112014270".
      *    小児科再診（乳幼児深夜）加算（６歳未満）
           03  FILLER          PIC X(09)   VALUE   "112014370".
      *    再診（夜間・早朝等）加算
           03  FILLER          PIC X(09)   VALUE   "112015570".
      *    時間外対応加算１
           03  FILLER          PIC X(09)   VALUE   "112016070".
      *    地域医療貢献加算
           03  FILLER          PIC X(09)   VALUE   "112015670".
      *    時間外対応加算３
           03  FILLER          PIC X(09)   VALUE   "112016170".
      *    明細書発行体制等加算
           03  FILLER          PIC X(09)   VALUE   "112015770".
      *    地域包括診療加算１
           03  FILLER          PIC X(09)   VALUE   "112021770".
      *    地域包括診療加算
           03  FILLER          PIC X(09)   VALUE   "112017270".
      *    認知症地域包括診療加算１
           03  FILLER          PIC X(09)   VALUE   "112021870".
      *    認知症地域包括診療加算
           03  FILLER          PIC X(09)   VALUE   "112017570".
      *    薬剤適正使用連携加算（再診）
           03  FILLER          PIC X(09)   VALUE   "112021970".
      *    妊婦加算（再診）
           03  FILLER          PIC X(09)   VALUE   "112022070".
      *    妊婦夜間加算（産科又は産婦人科再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112022170".
      *    妊婦休日加算（産科又は産婦人科再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112022270".
      *    妊婦深夜加算（産科又は産婦人科再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112022370".
      *
       01  TBL-NOSRYCD-R       REDEFINES   TBL-NOSRYCD-AREA.
            03  TBL-NOSRYCD    PIC X(09)   OCCURS   91.
      *
       01  TBL-NOSRYCD-MAX     PIC 9(03)   VALUE    91.
      *
      *   再診集計用診療行為テーブル
       01  TBL-SAISRYCD-AREA.
      *@   電話等再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023350".
      *@   同日電話等再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023450".
      *    再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016610".
      *    電話等再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016750".
      *@   電話等特定妥結率再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023650".
      *    同日再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016850".
      *    同日電話等再診（妥結率５割以下）
           03  FILLER          PIC X(09)   VALUE   "112016950".
      *@   同日電話等特定妥結率再診料（３０年３月以前継続）
           03  FILLER          PIC X(09)   VALUE   "112023750".
      *
      *    再診（乳幼児）加算
           03  FILLER          PIC X(09)   VALUE   "112000970".
      *
      *@   妊婦時間外加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021370".
      *@   妊婦休日加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021470".
      *@   妊婦深夜加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021570".
      *    時間外特例（再診）
           03  FILLER          PIC X(09)   VALUE   "112001410".
      *    再診（乳幼児時間外特例医療機関）加算
           03  FILLER          PIC X(09)   VALUE   "112015070".
      *@   妊婦時間外特例医療機関加算（再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112021670".
      *    小児科再診（乳幼児夜間）加算（６歳未満）
           03  FILLER          PIC X(09)   VALUE   "112014170".
      *    小児科再診（乳幼児休日）加算（６歳未満）
           03  FILLER          PIC X(09)   VALUE   "112014270".
      *    小児科再診（乳幼児深夜）加算（６歳未満）
           03  FILLER          PIC X(09)   VALUE   "112014370".
      *    再診（夜間・早朝等）加算
           03  FILLER          PIC X(09)   VALUE   "112015570".
      *    時間外対応加算１
           03  FILLER          PIC X(09)   VALUE   "112016070".
      *    地域医療貢献加算
           03  FILLER          PIC X(09)   VALUE   "112015670".
      *    時間外対応加算３
           03  FILLER          PIC X(09)   VALUE   "112016170".
      *    明細書発行体制等加算
           03  FILLER          PIC X(09)   VALUE   "112015770".
      *@   地域包括診療加算１
           03  FILLER          PIC X(09)   VALUE   "112021770".
      *    地域包括診療加算
           03  FILLER          PIC X(09)   VALUE   "112017270".
      *@   認知症地域包括診療加算１
           03  FILLER          PIC X(09)   VALUE   "112021870".
      *    認知症地域包括診療加算
           03  FILLER          PIC X(09)   VALUE   "112017570".
      *@   薬剤適正使用連携加算（再診）
           03  FILLER          PIC X(09)   VALUE   "112021970".
      *@   妊婦加算（再診）
           03  FILLER          PIC X(09)   VALUE   "112022070".
      *@   妊婦夜間加算（産科又は産婦人科再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112022170".
      *@   妊婦休日加算（産科又は産婦人科再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112022270".
      *@   妊婦深夜加算（産科又は産婦人科再診）（入院外）
           03  FILLER          PIC X(09)   VALUE   "112022370".
      *
       01  TBL-SAISRYCD-R      REDEFINES   TBL-SAISRYCD-AREA.
            03  TBL-SAISRYCD   PIC X(09)   OCCURS   32.
      *
       01  TBL-SAISRYCD-MAX    PIC 9(03)   VALUE    32.
      *
      *    固定値
       01  CONST-AREA.
      *    画像診断の単位の変更（枚→回）
           03  WRK-CONS-TANICD PIC 9(03)   VALUE   006.
           03  WRK-CONS-TANINAME
                               PIC X(02)   VALUE   "回".
           03  WRK-CONS-BUNKAKU-MOJISU
                               PIC 9(02)   VALUE   4.                    
           03  WRK-CONS-BUNKAKU
                               PIC X(04)   VALUE   "分画".
      *    薬剤料逓減
           03  CONS-TEIGEN     PIC X(09)   VALUE   "630010002".
      *    薬剤料逓減（８０／１００）（向精神薬多剤投与）
           03  CONS-TEIGEN2    PIC X(09)   VALUE   "630010005".
      *    薬剤料逓減（６０／１００）（紹介率が低い大病院３０日以上投薬）
           03  CONS-TEIGEN3    PIC X(09)   VALUE   "630010006".
      *    薬剤料減点（湿布薬薬剤料上限超）
           03  CONS-TEIGEN4    PIC X(09)   VALUE   "630010007".
      *    包括点数の治験減点分
           03  CONS-GENTEN     PIC X(09)   VALUE   "199000210".
      *    包括点数の公害補償法減点分
           03  CONS-GENTEN2    PIC X(09)   VALUE   "199000410".
      *---- (01.00.05) LINE ADD START --------------------------------
      *フリーコメント
      *    電話再診（　回）
           03  CONS-COMMENT1   PIC X(09)   VALUE   "840000001".
      *    同日複数再診（　回） 
           03  CONS-COMMENT2   PIC X(09)   VALUE   "840000002".
      *    フリーコメント 
           03  CONS-COMMENT3   PIC X(09)   VALUE   "810000001".
      *    複数診療科受診
           03  CONS-COMMENT4   PIC X(09)   VALUE   "099999901".
      *    集団検診から
           03  CONS-COMMENT5   PIC X(09)   VALUE   "820000003".
      *    老人検診から
           03  CONS-COMMENT6   PIC X(09)   VALUE   "820000004".
      *    学校検診から
           03  CONS-COMMENT7   PIC X(09)   VALUE   "820000005".
      *    健康診断から
           03  CONS-COMMENT8   PIC X(09)   VALUE   "820000006".
      *---- (01.00.05) LINE ADD END   --------------------------------
      *    皮下筋肉注射
           03  CONS-HIKAKIN    PIC X(09)   VALUE   "130000510".
      *    静脈内注射
           03  CONS-JYOMYAKU   PIC X(09)   VALUE   "130003510".
      *    （サ）
           03  CONS-RINYAK     PIC X(09)   VALUE   "820000046".
      *
      *初診加算
      *    初診（診療所）紹介患者加算
           03  WRK-CONS-SYOSIN1    PIC X(09)  VALUE "111003770".
      *    老人初診（診療所）紹介患者加算
           03  WRK-CONS-SYOSIN2    PIC X(09)  VALUE "111700470".
      *    小児科外来診療料（初診時診療時の紹介患者）加算
           03  WRK-CONS-SYOSIN5    PIC X(09)  VALUE "111010370".
      *    小児科外来診療料（紹介患者加算３）
           03  WRK-CONS-SYOSIN6    PIC X(09)  VALUE "111010870".
      *    小児科外来診療料（紹介患者加算４）
           03  WRK-CONS-SYOSIN7    PIC X(09)  VALUE "111010970".
      *    小児科外来診療料（紹介患者加算５）
           03  WRK-CONS-SYOSIN8    PIC X(09)  VALUE "111011070".
      *    小児科外来診療料（紹介患者加算６）
           03  WRK-CONS-SYOSIN9    PIC X(09)  VALUE "111011170".
      *    紹介患者加算１（病院）
           03  WRK-CONS-SYOSIN10   PIC X(09)  VALUE "111011270".
      *    紹介患者加算２（病院）
           03  WRK-CONS-SYOSIN11   PIC X(09)  VALUE "111011370".
      *    紹介患者加算３（病院）
           03  WRK-CONS-SYOSIN12   PIC X(09)  VALUE "111009970".
      *    紹介患者加算４（病院）
           03  WRK-CONS-SYOSIN13   PIC X(09)  VALUE "111010070".
      *    紹介患者加算５（病院）
           03  WRK-CONS-SYOSIN14   PIC X(09)  VALUE "111010170".
      *    紹介患者加算６（病院）
           03  WRK-CONS-SYOSIN15   PIC X(09)  VALUE "111010270".
      *    ＜老＞紹介患者加算１（病院）
           03  WRK-CONS-SYOSIN16   PIC X(09)  VALUE "111702770".
      *    ＜老＞紹介患者加算２（病院）
           03  WRK-CONS-SYOSIN17   PIC X(09)  VALUE "111702870".
      *    ＜老＞紹介患者加算３（病院）
           03  WRK-CONS-SYOSIN18   PIC X(09)  VALUE "111702370".
      *    ＜老＞紹介患者加算４（病院）
           03  WRK-CONS-SYOSIN19   PIC X(09)  VALUE "111702470".
      *    ＜老＞紹介患者加算５（病院）
           03  WRK-CONS-SYOSIN20   PIC X(09)  VALUE "111702570".
      *    ＜老＞紹介患者加算６（病院）
           03  WRK-CONS-SYOSIN21   PIC X(09)  VALUE "111702670".
      *    初診（乳幼児）（夜間）加算
           03  WRK-CONS-SYOSIN22   PIC X(09)  VALUE "111011570".
      *    初診（乳幼児）（休日）加算
           03  WRK-CONS-SYOSIN23   PIC X(09)  VALUE "111011670".
      *    初診（乳幼児）（深夜）加算
           03  WRK-CONS-SYOSIN24   PIC X(09)  VALUE "111011770".
      *    初診（時間外特例）加算
           03  WRK-CONS-SYOSIN25   PIC X(09)  VALUE "111000870".
      *    ＜老＞初診（時間外特例）加算
           03  WRK-CONS-SYOSIN26   PIC X(09)  VALUE "111700870".
      *    初診（乳幼児時間外特例医療機関）加算
           03  WRK-CONS-SYOSIN27   PIC X(09)  VALUE "111012270".
      *    同日初診
           03  WRK-CONS-SYOSIN28   PIC X(09)  VALUE "111011810".
      *    初診（夜間・早朝等）加算
           03  WRK-CONS-SYOSIN30   PIC X(09)  VALUE "111012470".
      *    初診料（同一日２科目：紹介状なし）
           03  WRK-CONS-SYOSIN32   PIC X(09)  VALUE "111012610".
      *    初診（同一日複数科受診時の２科目・妥結率５割以下）
           03  WRK-CONS-SYOSIN33   PIC X(09)  VALUE "111012810".
      *
      *    妊婦時間外加算（初診）
           03  WRK-CONS-SYOSIN34   PIC X(09)  VALUE "111012970".
      *    妊婦休日加算（初診）
           03  WRK-CONS-SYOSIN35   PIC X(09)  VALUE "111013070".
      *    妊婦深夜加算（初診）
           03  WRK-CONS-SYOSIN36   PIC X(09)  VALUE "111013170".
      *    妊婦時間外特例医療機関加算（初診）
           03  WRK-CONS-SYOSIN37   PIC X(09)  VALUE "111013270".
      *    妊婦加算（初診）
           03  WRK-CONS-SYOSIN38   PIC X(09)  VALUE "111013370".
      *    妊婦夜間加算（産科又は産婦人科初診）
           03  WRK-CONS-SYOSIN39   PIC X(09)  VALUE "111013470".
      *    妊婦休日加算（産科又は産婦人科初診）
           03  WRK-CONS-SYOSIN40   PIC X(09)  VALUE "111013570".
      *    妊婦深夜加算（産科又は産婦人科初診）
           03  WRK-CONS-SYOSIN41   PIC X(09)  VALUE "111013670".
      *    機能強化加算（初診）
           03  WRK-CONS-SYOSIN42   PIC X(09)  VALUE "111013770".
      *
      *    初診（他医療機関からの紹介状がない患者）
           03  WRK-CONS-SYOSIN101  PIC X(09)  VALUE "111012510".
      *    初診（妥結率５割以下）
           03  WRK-CONS-SYOSIN102  PIC X(09)  VALUE "111012710".
      *    電子化加算
           03  WRK-CONS-SYOSIN103  PIC X(09)  VALUE "111012370".
      *    初診料（新型コロナウイルス感染症・診療報酬上臨時的取扱）
           03  WRK-CONS-SYOSIN104  PIC X(09)  VALUE "111013850".
      *
      *    外来迅速検体検査加算
           03  WRK-CONS-GAIJINKEN      PIC X(09)   VALUE 
                                                   "160177770".
           03  WRK-CONS-GAIJINKEN-N    PIC 9(09)   VALUE 
                                                   160177770.
      *    尿一般
           03  WRK-CONS-NYOIPPAN-N     PIC 9(09)   VALUE 
                                                   160000310.
      *    検査の回数に集計しない診療行為
      *    採血（Ｂ−Ａ）
           03  WRK-CONS-SAIKETSU1      PIC X(09)   VALUE "160101210".
      *    採血（Ｂ−Ｖ）
           03 WRK-CONS-SAIKETSU2       PIC X(09)   VALUE "160095710".
      *    採血（Ｂ−Ｃ）
           03 WRK-CONS-SAIKETSU3       PIC X(09)   VALUE "160095810".
      *
      *    腰部固定帯加算
           03  WRK-CONS-YOUBUKASAN     PIC X(09)   VALUE "140037490".
      *    胸部固定帯加算
           03  WRK-CONS-KYOBUKASAN     PIC X(09)   VALUE "140040110".
      *    頸部固定帯加算
           03  WRK-CONS-KEIBUKASAN     PIC X(09)   VALUE "140052610".
      *    創外固定器使用加算
           03  WRK-CONS-SOGAIKASAN     PIC X(09)   VALUE "150266970".
      *    療養担当手当て
           03  WRK-CONS-RYOYOTEATE     PIC X(09)   VALUE "199000610".
      *
      *    フィルム料（乳幼児）加算
           03  WRK-CONS-FILM-YOUJI-KASAN
                                       PIC X(09)   VALUE 
                                                   "799990070".
      *    酸素補正率
           03  WRK-CONS-SANSO          PIC X(09)   VALUE 
                                                   "770020070".
      *    高気圧酸素加算
           03  WRK-CONS-SANSO2         PIC X(09)   VALUE 
                                                   "770030070".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       SPA-STS     =   2
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SPA-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TBL-ZAI-AREA
           INITIALIZE                  TBL-ZAIW-AREA
           INITIALIZE                  RECEERR
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0460"      TO  JOB-PGID
               MOVE    "点数摘要欄編集（労災・自賠責）"                      
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE01PARA
           INITIALIZE                      RECE01PARA
           MOVE    "RECE410"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE44PARA
           INITIALIZE                      RECE44PARA
           MOVE    "RECE44A"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE45PARA
           INITIALIZE                      RECE45PARA
           MOVE    "RECE45A"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE46PARA
           INITIALIZE                      RECE46PARA
           MOVE    "RECE460"           TO  RECE46PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE46PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE46PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE46PARA-BASENAME TO  SGETTEMP-BASENAMES  (5)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE44PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE45PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE46PARA
      *
           OPEN    I-O                 RECE41-FILE
      *
           OPEN    INPUT               RECE44-FILE
      *
           OPEN    INPUT               RECE45-FILE
      *
           OPEN    OUTPUT              RECE46X-FILE
           CLOSE                       RECE46X-FILE
      *
           OPEN    I-O                 RECE46-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *        行カウンタ初期化
           MOVE    ZERO                TO  WRK-LINENUM
      *    検査回数初期化
           MOVE    ZERO                TO  WRK-KENSA-KAISU
      *
           MOVE    ZERO                TO  CNT-SRYKBN60   
                                           WRK-GAIJINKEN-ZAINUM
           INITIALIZE                      WRK-RECE44-TABLE
                                           WRK-RECE44-MAX 
                                           WRK-SUM-KAISU-TABLE
                                           WRK-KENSA-MATOME-TABLE
                                           WRK-KENSA-MATOME-MAX
                                           WRK-MIN-KAISU-TABLE
                                           WRK-KENTAIKASAN-TABLE
      *
           INITIALIZE                      TBL-SORT11-INF
           INITIALIZE                      TBL-SORT12-INF
      *
      *    外来迅速検体検査加算算定日を確認
      ***  IF      RECE41-KENTAIKASANFLG     =   '1'
      ***      PERFORM 2009-KENTAIKASAN-KAKUNIN-SEC
      ***  END-IF
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                   )
               OR     ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               MOVE    ZERO                  TO  FLG-SRYCD1
      *
               INITIALIZE                      TBL-ZAIW-AREA
               MOVE    ZERO                TO  WRK-ZAIWMAX-G
               MOVE    ZERO                TO  WRK-TBLMAX
               MOVE    ZERO                TO  FLG-TEIGEN
                                               FLG-COMMENT
                                               FLG-COMMENT2
                                               FLG-COMMENT3
                                               FLG-HIKAKIN 
                                               FLG-GAIJINKEN
               MOVE    ZERO                TO  WRK-RECE45
                                               IDY                           
               MOVE    SPACE               TO  WRK-SRYSYUKBN
      *
               PERFORM 2002-TBLZAIW-TAIHI-SEC
      *
      *        レセプト０４剤編集前処理
               PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *
               IF      SPA-STS          =   ZERO
      * 
      *            レセプト０４ファイル編集処理
                   EVALUATE    RECE44-SRYKBN
      *                初診
                       WHEN    "11"
                           PERFORM 2003-SRYKBN11-HENSYU-SEC
      *                再診
                       WHEN    "12"
                           PERFORM 2004-SRYKBN12-HENSYU-SEC
      *                検査
                       WHEN    "60"
                       WHEN    "64"
                           PERFORM 2006-SRYKBN60-CHECK-SEC 
      *                それ以外
                       WHEN    OTHER
                           PERFORM 2005-SRYKBNELSE-HENSYU-SEC
                   END-EVALUATE
      *
                   ADD     1               TO  CNT-RECE44
      *
                   PERFORM 990-RECE44-NEXT-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
               END-IF     
           END-PERFORM
      *
           IF      SPA-STS         =   ZERO
      *        検査があるときまとめる
               IF      WRK-RECE44-MAX          >   ZERO
                   PERFORM 2007-SRYKBN60-HENSYU-SEC
               END-IF         
      *
      *        検査のとき回数を編集する
               IF      WRK-KENSA-KAISU        >   ZERO
                   EVALUATE  RECE41-HKNKBN
                     WHEN      "1"
                     WHEN      "2"
                     WHEN      "4"
                     WHEN      "6"
                       MOVE    WRK-KENSA-KAISU    TO  RECE41-KAISU  (29)
                     WHEN      "3"
                       MOVE    WRK-KENSA-KAISU    TO  RECE41-KAISU  (24)
                     WHEN      "5"
                       MOVE    WRK-KENSA-KAISU    TO  RECE41-KAISU  (32)
                   END-EVALUATE
                   REWRITE RECE41-REC
               END-IF
      *
      *        初診・再診の剤まとめ編集
               MOVE    ZERO      TO    FLG-TEIGEN
               PERFORM  VARYING  IDX6   FROM   1   BY  1
                         UNTIL   IDX6    >     4
                 IF      TBL-SORT11-FLG(IDX6)  =   1
                   PERFORM 2010-SYOSAIMATOME-HENSYU1-SEC
                 END-IF
               END-PERFORM
               PERFORM  VARYING  IDX6   FROM   1   BY  1
                         UNTIL   IDX6    >    32
                 IF      TBL-SORT12-FLG(IDX6)  =   1
                   PERFORM 2010-SYOSAIMATOME-HENSYU2-SEC
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE41-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF    
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４より剤編集前処理
      *****************************************************************
       2001-RECE44-ZAIHEN-MAE-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
           MOVE    "＊"                TO  WRK-ZAI-MK
           MOVE    ZERO                TO  WRK-CLM
      *    点数・回数セット
           MOVE    RECE44-ZAITEN       TO  WRK-ZAITENX
           MOVE    RECE44-ZAIKAISU     TO  WRK-ZAIKAISUX
      *    必要有効桁数・開始位置・回数編集
           PERFORM 810-TENSUKAI-HEN-SEC
      *
           .
       2001-RECE44-ZAIHEN-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブル退避処理
      *****************************************************************
       2002-TBLZAIW-TAIHI-SEC    SECTION.
      *
      *    パラメータ０５ファイルより編集
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                   )
               OR     ( FLG-RECE45           =   1                   )
               OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
               OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
               OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
               OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
               OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
               OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
               OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
               OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE45-SRYCD(IDX) =   SPACE )
                         OR      ( SPA-STS           =   2     ) 
                 IF      RECE45-SIKFLG (IDX)         =   "1"        
                 AND     RECE45-RECEPTFLG(IDX)       =   SPACE
      *
                   IF      RECE45-SRYCD (IDX) (1:1)  =   "1"
                       IF      RECE45-DATAKBN(IDX)         =   "2"
                           MOVE    1                 TO  FLG-SRYCD1
                       END-IF
                   ELSE
                       MOVE    1                     TO  FLG-SRYCD1
                   END-IF
      *              
                   IF      RECE44-SRYKBN     <   "31"
                       MOVE    1             TO  IDY
                   ELSE                          
                     IF    RECE44-ZAIKBN     =    1
                       MOVE    2             TO  IDY
                     ELSE
                       EVALUATE    RECE45-SRYCD (IDX) (1:1)
                           WHEN    "0"
                             IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                               IF      RECE45-SRYCD (IDX) (1:3) = "059"
                                   IF   ( RECE44-SRYKBN     =   "70" )
                                   AND  ( RECE45-DATAKBN (IDX)
                                                            =   "3"  )
                                       MOVE    3             TO  IDY
                                   ELSE
                                       MOVE    4             TO  IDY
                                   END-IF
                               ELSE
                                 IF    RECE45-SRYCD (IDX) (1:3) = "058"
                                       MOVE    4             TO  IDY
                                 ELSE
                                   IF  RECE45-SRYCD (IDX) (1:3) = "008"
                                     IF   RECE45-SRYSYUKBN  =  "334"
                                         MOVE    5         TO  IDY
                                     ELSE
                                       IF      IDY          =   ZERO
                                         MOVE    2         TO  IDY
                                       END-IF
                                     END-IF
                                   ELSE
                                     IF      IDY           =   ZERO
                                         MOVE    1         TO  IDY
                                     END-IF
                                   END-IF
                                 END-IF
                               END-IF
                             ELSE
                               IF      RECE45-SRYCD (IDX) (1:3) = "059"
                                       MOVE    4             TO  IDY
                               ELSE
                                 IF    RECE45-SRYCD (IDX) (1:3) = "058"
                                       MOVE    4             TO  IDY
                                 ELSE
                                   IF  RECE45-SRYCD (IDX) (1:3) = "008"
                                      IF   RECE45-SRYSYUKBN  =  "334"
                                          MOVE    5         TO  IDY
                                      ELSE
                                        IF      IDY          =   ZERO
                                          MOVE    2         TO  IDY
                                        END-IF
                                      END-IF
                                   ELSE
                                      IF      IDY           =   ZERO
                                          MOVE    1         TO  IDY
                                      END-IF
                                   END-IF
                                 END-IF
                               END-IF
                             END-IF
                           WHEN    "8"
                               IF   RECE45-SRYSYUKBN  =  "334"
                                   MOVE    5         TO  IDY
                               ELSE
                                 IF (     IDY            =   ZERO      )
      *****                      OR ( RECE45-SRYCD (IDX) = CONS-RINYAK )
                                   MOVE    2         TO  IDY
                                 END-IF
                               END-IF
                           WHEN    "1"
                               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                                   MOVE    3                 TO  IDY
                               ELSE
                                   IF  RECE45-SRYCD (IDX)    =
                                           WRK-CONS-YOUBUKASAN
                                        OR WRK-CONS-KYOBUKASAN
                                        OR WRK-CONS-KEIBUKASAN
                                        OR WRK-CONS-SOGAIKASAN
                                     IF  RECE41-KOTEIKBN = SPACE OR "2"
                                       MOVE    4             TO  IDY
                                     ELSE
                                       MOVE    3             TO  IDY
                                     END-IF
                                   ELSE
                                     IF  RECE45-SRYCD (IDX)  =
                                           WRK-CONS-RYOYOTEATE
                                       IF ((RECE41-HKNKBN   = "4") AND
                                           (RECE41-JIBAIKBN = "2"))
                                       OR ( RECE41-HKNKBN   = "6" )
                                         MOVE  4             TO  IDY
                                       ELSE
                                         MOVE  3             TO  IDY
                                       END-IF
                                     ELSE
                                       MOVE    3             TO  IDY
                                     END-IF
                                   END-IF
                               END-IF
                           WHEN    "6"
                               MOVE    5             TO  IDY
                           WHEN    "7"
                             IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                               IF    ( RECE44-SRYKBN     =   "70" )
                               AND   ( RECE45-DATAKBN (IDX)
                                                         =   "3"  )
                                   MOVE    3                 TO  IDY
                               ELSE
                                   IF  RECE45-SRYCD (IDX)    =
                                           WRK-CONS-FILM-YOUJI-KASAN
                                       MOVE    3             TO  IDY
                                   ELSE
                                       MOVE    4             TO  IDY
                                   END-IF
                               END-IF
                             ELSE
                                       MOVE    4             TO  IDY
                             END-IF
                           WHEN    OTHER
                               DISPLAY "ORCR0460 SRYCD="
                                                 RECE45-SRYCD (IDX) 
                               MOVE    "診療行為コードが範囲外です"
                                                 TO  WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               MOVE    2             TO  SPA-STS
                               GO  TO  2002-TBLZAIW-TAIHI-EXT
                       END-EVALUATE
                     END-IF
                   END-IF            
      *
                   IF      RECE45-KENTAICOMMENT (IDX)   =   1
                       MOVE    RECE45-NAME (IDX)        TO
                                             WRK-KENTAICOMMENT-NAME
                       INSPECT WRK-KENTAICOMMENT-NAME
                               REPLACING  ALL  "〈"  BY  "（"
                                          ALL  "〉"  BY  "）"
                       MOVE    RECE45-YUKOKETA (IDX)    TO
                                             WRK-KENTAICOMMENT-YUKOKETA
                       MOVE    RECE45-SRYCD (IDX)       TO
                                             WRK-KENTAICOMMENT-SRYCD
                   ELSE
      *
                   ADD     1                 TO  WRK-ZAIWMAX (IDY)
                                                 WRK-ZAIWMAX (6)
                   IF      WRK-ZAIWMAX (IDY) >   TBL-ZAI-MAX 
                       MOVE    "テーブルがオーバーしました"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    2                 TO  SPA-STS
                       GO  TO  2002-TBLZAIW-TAIHI-EXT
                   ELSE
      *                先頭の診療行為コード編集              
                       IF      WRK-RECE45        =   ZERO
                         IF  ( RECE45-SRYCD(IDX)(1:1)  =  "8"       )
                         OR  ((RECE45-SRYCD(IDX)(1:3)  NOT =  "059") AND
                              (RECE45-SRYCD(IDX)(1:1)  =  "0"      ))
                         OR  ( RECE44-SAFLG       NOT  =  SPACE     )
                         OR  ( RECE44-ZAIKBN           =   1        )
                           CONTINUE
                         ELSE
                           MOVE    RECE45-SRYCD (IDX)
                                                 TO  WRK-ZAIW-SRYCD1 
                           MOVE    RECE45-CDKBN-KBN(IDX)
                                                 TO  WRK-ZAIW-CDKBN-KBN
                           MOVE    RECE45-CDKBN-SYO(IDX)
                                                 TO  WRK-ZAIW-CDKBN-SYO
                           MOVE    RECE45-CDKBN-BU(IDX)
                                                 TO  WRK-ZAIW-CDKBN-BU
                           MOVE    RECE45-CDKBN-KBNNUM(IDX)
                                         TO  WRK-ZAIW-CDKBN-KBNNUM
                           MOVE    RECE45-CDKBN-KBNNUM-EDA(IDX)
                                         TO  WRK-ZAIW-CDKBN-KBNNUM-EDA
                           MOVE    RECE45-CDKBN-KOUBAN(IDX)
                                         TO  WRK-ZAIW-CDKBN-KOUBAN
                           MOVE    RECE45-KOUHYOJYUNNUM(IDX)
                                         TO  WRK-ZAIW-KOUHYOJYUNNUM
                           MOVE    RECE45-KNSJISKBN(IDX)
                                         TO  WRK-ZAIW-KNSJISKBN
                           MOVE    1             TO  WRK-RECE45
                         END-IF
                       END-IF
      *
                       MOVE    RECE45-SRYSYUKBN  TO  WRK-SRYSYUKBN
      *                                                         
                       MOVE    WRK-ZAIWMAX (IDY) TO  IDZ
                       MOVE    RECE45-SRYACTINF (IDX)
                                     TO  TBL-ZAIW-SRYACTINF (IDY IDZ)
                       MOVE    RECE45-NAMEINF   (IDX)
                                     TO  TBL-ZAIW-NAMEINF   (IDY IDZ)
                       IF  RECE45-INGAIFLG-RECE(IDX) = "1"
                         MOVE  RECE45-GENERICNAME(IDX)
                                     TO  TBL-ZAIW-NAME(IDY IDZ)
                         MOVE  RECE45-GENERICYUKOKETA(IDX)
                                     TO  TBL-ZAIW-YUKOKETA(IDY IDZ)
                       END-IF
                       MOVE    RECE45-RECEHENFLG(IDX)
                                     TO  TBL-ZAIW-RECEHENFLG(IDY IDZ)
                       MOVE    RECE45-SRYSYUKBN  TO
                                     TBL-ZAIW-SRYSYUKBN-INPUT(IDY IDZ)
                       MOVE    RECE45-TNSSRYKBN(IDX)
                                     TO  TBL-ZAIW-TNSSRYKBN(IDY IDZ)
                       IF      WRK-KENTAICOMMENT-SRYCD  NOT =  ZERO
                           STRING  TBL-ZAIW-NAME (IDY IDZ)
                                     DELIMITED   BY  SPACE
                                   WRK-KENTAICOMMENT-NAME
                                     DELIMITED   BY  SPACE
                                   INTO  WRK-KENTAICOMMENT-NAME-TEMP
                           END-STRING
                           MOVE    WRK-KENTAICOMMENT-NAME-TEMP
                                     TO  TBL-ZAIW-NAME (IDY IDZ)
                           COMPUTE TBL-ZAIW-YUKOKETA (IDY IDZ)  =
                                   TBL-ZAIW-YUKOKETA (IDY IDZ)  +
                                   WRK-KENTAICOMMENT-YUKOKETA
                           MOVE    WRK-KENTAICOMMENT-SRYCD      TO
                                  TBL-ZAIW-KENTAICOMMENT-SRYCD (IDY IDZ)
                           MOVE    ZERO          TO
                                     WRK-KENTAICOMMENT-YUKOKETA
                                     WRK-KENTAICOMMENT-SRYCD
                           MOVE    SPACE         TO
                                     WRK-KENTAICOMMENT-NAME
                                     WRK-KENTAICOMMENT-NAME-TEMP
                       END-IF
     *
                       IF      RECE45-SRYCD (IDX) (1:1)
                                                 =   "1"
                       AND     RECE44-SRYKBN     =   "70"
                       AND     RECE45-TANICD(IDX)
                                                 =   WRK-CONS-TANICD
                           MOVE    WRK-CONS-TANINAME TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                       END-IF                     
      *
                       IF      RECE45-SRYCD (IDX) (1:1)  =  "7"
                          IF    ( RECE44-SRYKBN     =   "70" )
                          AND   ( RECE45-DATAKBN (IDX)
                                                    =   "3"  )
                            IF  TBL-ZAIW-TANINAME  (IDY IDZ)  
                                                    =   SPACE
                                  MOVE    "＊"  TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                            END-IF
                          ELSE
                            IF      RECE45-SRYCD (IDX)    =
                                     WRK-CONS-FILM-YOUJI-KASAN  OR
                                     WRK-CONS-SANSO             OR
                                     WRK-CONS-SANSO2
                               CONTINUE
                            ELSE
                               IF  TBL-ZAIW-TANINAME  (IDY IDZ)  
                                                    =   SPACE
                                  MOVE    "＊"  TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                               END-IF
                            END-IF
                          END-IF
                       END-IF
      *
                       IF      RECE45-SRYCD (IDX) (1:1)  =  "6"
                          IF   RECE45-SRYCD (IDX) (1:2)  =  "63"
                               CONTINUE
                          ELSE
                            IF  TBL-ZAIW-TANINAME  (IDY IDZ)  
                                                    =   SPACE
                                  MOVE    "＊"  TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                            END-IF
                          END-IF
                       END-IF
      *
      *---- (01.00.05) LINE ADD START --------------------------------
                       IF      RECE45-SRYCD(IDX) =   CONS-COMMENT1   OR
                                                     CONS-COMMENT2   OR
                                                     CONS-COMMENT3   OR
                                                     CONS-COMMENT4 
                           MOVE    1             TO  FLG-COMMENT
                       END-IF 
                       IF      RECE45-SRYCD(IDX) =   CONS-COMMENT5   OR 
                                                     CONS-COMMENT6   OR 
                                                     CONS-COMMENT7   OR 
                                                     CONS-COMMENT8      
                           MOVE    1             TO  FLG-COMMENT2       
                       END-IF                                           
                       IF     (RECE45-SRYCD(IDX)(1:1)   =   "8"  )
                       OR     (RECE45-SRYCD(IDX)(1:3)   =   "008")
                           MOVE    1             TO  FLG-COMMENT3
                       END-IF
      *---- (01.00.05) LINE ADD END --------------------------------
      *
                       IF      RECE45-SRYCD(IDX) =   CONS-TEIGEN
                                                 OR  CONS-TEIGEN2
                                                 OR  CONS-TEIGEN3
                                                 OR  CONS-TEIGEN4
                                                 OR  CONS-GENTEN
                                                 OR  CONS-GENTEN2
                           MOVE    1             TO  FLG-TEIGEN
                       END-IF 
                       IF     (RECE44-SRYKBN     =   "31" OR "32" )
                       AND    (RECE45-SRYCD(IDX) =   CONS-HIKAKIN
                                                 OR  CONS-JYOMYAKU)
                           IF  (RECE44-YKZTEN     =   ZERO)
                           AND (RECE44-KIZAITEN   =   ZERO)
                               CONTINUE
                           ELSE
                               MOVE    1         TO  FLG-HIKAKIN
                           END-IF
                       END-IF  
                   END-IF
      *
                   END-IF
                 END-IF  
               END-PERFORM
               ADD     1                 TO  CNT-RECE45
      *
               PERFORM 990-RECE45-NEXT-SEC
           END-PERFORM
      *
           .
       2002-TBLZAIW-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分（１１）編集処理
      *****************************************************************
       2003-SRYKBN11-HENSYU-SEC    SECTION.
      *
      *    剤　明細テーブルチェック
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    ZERO              TO  WRK-TENSUGOKEI
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       1  
               PERFORM VARYING IDX2    FROM    1   BY  1
                         UNTIL IDX2    >   WRK-ZAIWMAX (IDX1)  
                   MOVE    IDX1          TO  WRK-WIDX1
                   MOVE    IDX2          TO  WRK-WIDX2
                   PERFORM 20031-SRYKBN11-CHECK-SEC
                   IF  FLG-NOPRT         =   ZERO
      *            レセプト０５剤編集処理
                       PERFORM 430-TBLZAI-HEN-SEC
                   END-IF
               END-PERFORM
           END-PERFORM    
      *
           IF  WRK-TBLMAX           >   ZERO
      *        剤明細数と印字対象明細数が異なる場合
               IF  WRK-TBLMAX       NOT =   RECE44-MEISAISU
                   PERFORM 400-TENSU-SAIKEISAN-SEC
               END-IF
      *        点数・回数セット処理
               MOVE    WRK-TBLMAX       TO  IDY
               PERFORM 440-TNSKAI-SET-SEC
               MOVE    IDY              TO  WRK-TBLMAX
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2003-SRYKBN11-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（１１） 処理
      *****************************************************************
       20031-SRYKBN11-CHECK-SEC        SECTION.
      * 
           MOVE    ZERO          TO  FLG-NOPRT
      *
      *    服用方法でないこと
           IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3)   =   "001"
               MOVE    1             TO  FLG-NOPRT
           END-IF
      *    紹介患者加算服用方法でないこと
           IF   RECE44-JIHOKBN  =  SPACE
             IF  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                                            =   WRK-CONS-SYOSIN1
                                            OR  WRK-CONS-SYOSIN2
                                            OR  WRK-CONS-SYOSIN5
                                            OR  WRK-CONS-SYOSIN6
                                            OR  WRK-CONS-SYOSIN7
                                            OR  WRK-CONS-SYOSIN8
                                            OR  WRK-CONS-SYOSIN9
                                            OR  WRK-CONS-SYOSIN10
                                            OR  WRK-CONS-SYOSIN11
                                            OR  WRK-CONS-SYOSIN12
                                            OR  WRK-CONS-SYOSIN13
                                            OR  WRK-CONS-SYOSIN14
                                            OR  WRK-CONS-SYOSIN15
                                            OR  WRK-CONS-SYOSIN16
                                            OR  WRK-CONS-SYOSIN17
                                            OR  WRK-CONS-SYOSIN18
                                            OR  WRK-CONS-SYOSIN19
                                            OR  WRK-CONS-SYOSIN20
                                            OR  WRK-CONS-SYOSIN21
                                            OR  WRK-CONS-SYOSIN22
                                            OR  WRK-CONS-SYOSIN23
                                            OR  WRK-CONS-SYOSIN24
                                            OR  WRK-CONS-SYOSIN25
                                            OR  WRK-CONS-SYOSIN26
                                            OR  WRK-CONS-SYOSIN27
                                            OR  WRK-CONS-SYOSIN28
                                            OR  WRK-CONS-SYOSIN30
                                            OR  WRK-CONS-SYOSIN32
                                            OR  WRK-CONS-SYOSIN33
                                            OR  WRK-CONS-SYOSIN34
                                            OR  WRK-CONS-SYOSIN35
                                            OR  WRK-CONS-SYOSIN36
                                            OR  WRK-CONS-SYOSIN37
                                            OR  WRK-CONS-SYOSIN38
                                            OR  WRK-CONS-SYOSIN39
                                            OR  WRK-CONS-SYOSIN40
                                            OR  WRK-CONS-SYOSIN41
                                            OR  WRK-CONS-SYOSIN42
                                            OR  CONS-COMMENT5         
                                            OR  CONS-COMMENT6         
                                            OR  CONS-COMMENT7         
                                            OR  CONS-COMMENT8         )
             OR  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:1)  = "8" AND
                  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                                                     NOT = "830000020")
             OR  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3)  = "008"  )
             OR  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                           = "101989010" OR "101989020" OR "101989060")
               CONTINUE
             ELSE
               MOVE    1             TO  FLG-NOPRT
             END-IF
           END-IF
      *
      *    識別が点数の場合
           IF  FLG-NOPRT                     =   ZERO
               IF      TBL-ZAIW-TENSIKIBETU (WRK-WIDX1 WRK-WIDX2)
                                             =   3
                   ADD     TBL-ZAIW-TEN (WRK-WIDX1 WRK-WIDX2)
                                             TO  WRK-TENSUGOKEI
               END-IF
           END-IF
      *
           IF   RECE44-JIHOKBN  =  SPACE
            IF TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) =
                                            WRK-CONS-SYOSIN101  OR
                                            WRK-CONS-SYOSIN102  OR
                                            WRK-CONS-SYOSIN103  OR
                                            WRK-CONS-SYOSIN104
      *
               EVALUATE  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                 WHEN      WRK-CONS-SYOSIN103
                     MOVE   1    TO   IDX6
                 WHEN      WRK-CONS-SYOSIN104
                     MOVE   2    TO   IDX6
                 WHEN      WRK-CONS-SYOSIN101
                     MOVE   3    TO   IDX6
                 WHEN      WRK-CONS-SYOSIN102
                     MOVE   4    TO   IDX6
               END-EVALUATE
      *
               MOVE     1             TO  TBL-SORT11-FLG(IDX6)
               MOVE  TBL-ZAIW-TEN (WRK-WIDX1 WRK-WIDX2)
                                      TO  TBL-SORT11-TEN(IDX6)
               MOVE  TBL-ZAIW-YUKOKETA(WRK-WIDX1 WRK-WIDX2)
                                      TO  TBL-SORT11-YUKOKETA(IDX6)
               MOVE  TBL-ZAIW-NAME(WRK-WIDX1 WRK-WIDX2)
                                      TO  TBL-SORT11-NAME(IDX6)
               ADD   RECE44-ZAIKAISU  TO  TBL-SORT11-ZAIKAISU(IDX6)
               MOVE  RECE44-HKNCOMBI  TO  TBL-SORT11-HKNCOMBI(IDX6)
      *
            END-IF
           END-IF
      *
           .
       20031-SRYKBN11-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４ファイル  診療区分（１２）編集処理
      *****************************************************************
       2004-SRYKBN12-HENSYU-SEC    SECTION.
      *
      *    剤　明細テーブルチェック
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    ZERO              TO  WRK-TENSUGOKEI
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       1  
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >   WRK-ZAIWMAX (IDX1)  
                   MOVE    IDX1          TO  WRK-WIDX1
                   MOVE    IDX2          TO  WRK-WIDX2
                   PERFORM 20041-SRYKBN12-CHECK-SEC
                   IF  FLG-NOPRT         =   ZERO
      *                レセプト０５剤編集処理
                       PERFORM 430-TBLZAI-HEN-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM
      *
           IF  WRK-TBLMAX           >   ZERO
      *        剤明細数と印字対象明細数が異なる場合
               IF  WRK-TBLMAX       NOT =   RECE44-MEISAISU
                   PERFORM 400-TENSU-SAIKEISAN-SEC
               END-IF
      *        点数・回数セット処理
               MOVE    WRK-TBLMAX       TO  IDY
               PERFORM 440-TNSKAI-SET-SEC
               MOVE    IDY              TO  WRK-TBLMAX
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2004-SRYKBN12-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（１２） 処理
      *****************************************************************
       20041-SRYKBN12-CHECK-SEC        SECTION.
      * 
           MOVE    ZERO              TO  FLG-NOPRT
      *
      *    服用方法でないこと
           IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3)
                                     =   "001"
               MOVE    1                 TO  FLG-NOPRT
           END-IF
      *
      *    診療行為テーブルに存在した場合、印字しないこと
           IF   RECE44-JIHOKBN  =  SPACE
             PERFORM   VARYING   IDY   FROM    1   BY  1
                       UNTIL   ( IDY           >   TBL-NOSRYCD-MAX )
                       OR      ( FLG-NOPRT     =    1              )
               IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) 
                                         =   TBL-NOSRYCD (IDY)
                   IF  ( RECE41-SRYYM   <  "200804"              )
                   AND ( TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                            =   "112015570"  OR  "112001410"  OR
                                "112014170"  OR  "112014270"  OR
                                "112014370"  OR  "112015070"     )
                       MOVE    ZERO          TO  FLG-NOPRT
                   ELSE
                       MOVE    1             TO  FLG-NOPRT
                   END-IF
      *
      *            ＊外来管理加算の場合
                   IF  ((TBL-ZAIW-SRYCD(WRK-WIDX1 WRK-WIDX2)
                           =  "101120070" OR "101120080"     ) AND
                        (RECE41-SRYYM   <=  "201306"         ))
                   OR  ((TBL-ZAIW-SRYCD(WRK-WIDX1 WRK-WIDX2)
                           =  "101120020"                    ) AND
                        (RECE41-SRYYM   >=  "201307"         ))
                     IF  RECE41-GAIRAIKSN-PRTKBN   =   "1"
                         MOVE    ZERO          TO  FLG-NOPRT
                     END-IF
                   END-IF
                   IF  ((TBL-ZAIW-SRYCD(WRK-WIDX1 WRK-WIDX2)
                           =  "101120070" OR "101120080"     ) AND
                        (RECE41-SRYYM   >=  "201307"         ))
                   OR  ((TBL-ZAIW-SRYCD(WRK-WIDX1 WRK-WIDX2)
                           =  "101120020"                    ) AND
                        (RECE41-SRYYM   <=  "201306"         ))
                         MOVE    ZERO          TO  FLG-NOPRT
                   END-IF
      *            一つの剤に他の明細も存在する場合
      ***          IF  RECE44-MEISAISU       >   1
      ***              MOVE    ZERO          TO  FLG-NOPRT
      ***          END-IF
      *            ＊再診ダミーの場合
                   IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)          
                                         =   "099120001"             
                       MOVE    1             TO  FLG-NOPRT           
                   END-IF                                            
               END-IF
             END-PERFORM
           END-IF
      *
           IF  FLG-NOPRT             =   ZERO
      *        識別が点数の場合
               IF  TBL-ZAIW-TENSIKIBETU (WRK-WIDX1 WRK-WIDX2)
                                     =   3
                   ADD     TBL-ZAIW-TEN (WRK-WIDX1 WRK-WIDX2)
                                         TO  WRK-TENSUGOKEI
               END-IF
           END-IF    
      *
           IF   RECE44-JIHOKBN  =  SPACE
             MOVE     ZERO     TO   FLG-ARI
             PERFORM  VARYING  IDY  FROM  1  BY  1
                       UNTIL  (IDY      >  TBL-SAISRYCD-MAX)
                        OR    (FLG-ARI  =    1             )
               IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                                        =  TBL-SAISRYCD(IDY)
                 MOVE  1               TO TBL-SORT12-FLG(IDY)
                 MOVE  TBL-ZAIW-TEN (WRK-WIDX1 WRK-WIDX2)
                                       TO TBL-SORT12-TEN(IDY)
                 MOVE  TBL-ZAIW-YUKOKETA(WRK-WIDX1 WRK-WIDX2)
                                       TO TBL-SORT12-YUKOKETA(IDY)
                 MOVE  TBL-ZAIW-NAME(WRK-WIDX1 WRK-WIDX2)
                                       TO TBL-SORT12-NAME(IDY)
                 ADD   RECE44-ZAIKAISU TO TBL-SORT12-ZAIKAISU(IDY)
                 MOVE  RECE44-HKNCOMBI TO TBL-SORT12-HKNCOMBI(IDY)
                 MOVE  1               TO FLG-ARI
                 IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) = "112000970"
                   IF  (RECE41-HKNKBN     = "4")
                   AND (WRK-PARA-RECEKBN  = "4")
                     CONTINUE
                   ELSE
                     MOVE  ZERO        TO TBL-SORT12-FLG(IDY)
                   END-IF
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       20041-SRYKBN12-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４ファイル  診療区分（その他）編集処理
      *****************************************************************
       2005-SRYKBNELSE-HENSYU-SEC    SECTION.
      *    処方のとき
           IF  RECE44-SRYKBN         =   "25"
                   PERFORM 20052-SRYKBNELSE-CHECK-SEC
           ELSE            
      *
      *        検査のとき剤が一つ以上なら”、”で編集
               IF  RECE44-SRYKBN         =   "60" OR "64"
                   PERFORM 20051-SRYKBNELSE-KENSA-SEC
               ELSE 
                   PERFORM 20052-SRYKBNELSE-CHECK-SEC
               END-IF
           END-IF    
      *
           IF  WRK-TBLMAX           >   ZERO
               IF      RECE44-SRYKBN      >=  "33"
               AND     RECE44-ZAIKBN   NOT =   1
               AND     RECE44-JIHOKBN      =   SPACE
                   CONTINUE
               ELSE        
                   MOVE    WRK-TBLMAX       TO  IDY
                   PERFORM 440-TNSKAI-SET-SEC
                   MOVE    IDY              TO  WRK-TBLMAX
               END-IF    
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2005-SRYKBNELSE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブル編集　 診療区分（その他）検査のとき処理
      *****************************************************************
       20051-SRYKBNELSE-KENSA-SEC      SECTION.
      *
           MOVE    WRK-ZAIWMAX (3)     TO  WRK-KENSA-MAX
           MOVE    ZERO                TO  WRK-TNS-HOUKNSKBN
                                           WRK-KOUMOKUSU
      *     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-ZAIWMAX (3)
               IF    ( TBL-ZAIW-HOUKNSKBN(3 IDX) =   11   )
               AND   ( TBL-ZAIW-KZMCOMPSIKIBETU  (3 IDX)
                                                 >   ZERO ) 
      *            数量編集
                   MOVE    3               TO  WRK-WIDX1
                   MOVE    IDX             TO  WRK-WIDX2
                   PERFORM 4301-SURYO-HEN1-SEC
                   MOVE    SPACE           TO  TBL-NAME (IDX)
                   STRING  TBL-ZAIW-NAME     (3 IDX)
                                              DELIMITED  BY  SPACE
                           "（"               DELIMITED  BY  SIZE
                           WRK-SRYSURYOW      DELIMITED  BY  SPACE
                           TBL-ZAIW-TANINAME(3 IDX)
                                              DELIMITED  BY  SPACE
                           "）"               DELIMITED  BY  SIZE
                           INTO               TBL-NAME (IDX)
                   END-STRING
                   MOVE    ZERO               TO  TBL-YUKOKETA (IDX)
                   MOVE    SPACE   TO   TBL-ZAIW-TANINAME(3 IDX)
               ELSE
                   MOVE    TBL-ZAIW-NAMEINF (3 IDX)
                                              TO  TBL-NAMEINF (IDX)
               END-IF
               MOVE    TBL-ZAIW-SRYSURYO(3 IDX)  TO  TBL-SRYSURYO(IDX)
               MOVE    TBL-ZAIW-TANICD  (3 IDX)  TO  TBL-TANICD  (IDX)
               MOVE    TBL-ZAIW-TANIMOJISU(3 IDX)
                                             TO  TBL-TANIMOJISU(IDX)
               MOVE    TBL-ZAIW-TANINAME(3 IDX)  TO  TBL-TANINAME(IDX)
               MOVE    TBL-ZAIW-SRYCD   (3 IDX)  TO  TBL-SRYCD   (IDX)
               MOVE    TBL-ZAIW-DATAKBN (3 IDX)  TO  TBL-DATAKBN (IDX)
               MOVE    TBL-ZAIW-HOUKNSKBN(3 IDX) TO  TBL-HOUKNSKBN(IDX)
               IF      TBL-YUKOKETA (IDX)      =   ZERO
                   PERFORM VARYING IDZ FROM   300 BY  -1
                           UNTIL   IDZ <      1
                       IF      TBL-NAME (IDX) (IDZ:1)    NOT =   SPACE
                           COMPUTE TBL-YUKOKETA (IDX)    =
                                   IDZ     /   2
                           MOVE    1          TO  IDZ
                       END-IF
                   END-PERFORM
               END-IF             
           END-PERFORM
      *
           COMPUTE WRK-ZAIWMAX (6)   =   WRK-ZAIWMAX  (6)     -   
                                         WRK-ZAIWMAX  (3) 
           INITIALIZE                    TBL-ZAIW-OCC (3)
           MOVE    ZERO              TO  WRK-ZAIWMAX  (3)
      *
      *    剤が一つ以上のとき’、’で名称を編集する
      *
           MOVE    1                 TO  IDX
               
           PERFORM UNTIL   IDX     >       WRK-KENSA-MAX
               IF      TBL-HOUKNSKBN(IDX)   =   ZERO
               OR      FLG-SRYCD1           =    1
                   IF     (TBL-HOUKNSKBN(IDX)  NOT =  ZERO)
                   AND    (FLG-SRYCD1              =   1  )
                     IF    IDX  =   1
                       MOVE  TBL-HOUKNSKBN(IDX)  TO  WRK-TNS-HOUKNSKBN
                     END-IF
                     IF  TBL-DATAKBN (IDX)   NOT =   "2"
                       ADD     1                 TO  WRK-KOUMOKUSU
                     END-IF
                   END-IF
                   ADD     1                        TO  WRK-ZAIWMAX (3)
                   MOVE    WRK-ZAIWMAX (3)          TO  IDZ
                   MOVE    TBL-NAMEINF (IDX)   TO
                                    TBL-ZAIW-NAMEINF (3 IDZ)
                   MOVE    TBL-SRYSURYO(IDX)   TO
                                    TBL-ZAIW-SRYSURYO(3 IDZ)
                   MOVE    TBL-TANICD  (IDX)   TO
                                    TBL-ZAIW-TANICD  (3 IDZ)
                   MOVE    TBL-TANIMOJISU(IDX) TO
                                    TBL-ZAIW-TANIMOJISU(3 IDZ)
                   MOVE    TBL-TANINAME(IDX)   TO
                                    TBL-ZAIW-TANINAME(3 IDZ)
                   MOVE    TBL-SRYCD   (IDX)   TO
                                    TBL-ZAIW-SRYCD   (3 IDZ)
                   MOVE    TBL-DATAKBN (IDX)   TO
                                    TBL-ZAIW-DATAKBN (3 IDZ)
                   MOVE    TBL-HOUKNSKBN(IDX)  TO
                                    TBL-ZAIW-HOUKNSKBN(3 IDZ)
                   IF      (FLG-GAIJINKEN  =   1) 
      ****           AND   (IDX            =   WRK-KENSA-MAX) 
                     AND   (TBL-SRYCD (IDX)(1:1)
                                       NOT =   "8")
                     AND   (TBL-SRYCD (IDX)(1:3)
                                       NOT =   "008")
                           PERFORM 200511-GJK-DAY-ADD-SEC
                   END-IF
                   ADD     1                   TO  IDX                     
               ELSE
                   IF      IDX             =   1
                       MOVE    TBL-HOUKNSKBN(IDX)
                                           TO  WRK-TNS-HOUKNSKBN         
                   END-IF                                
                   MOVE    ZERO            TO  WRK-YUKOKETA
                   MOVE    SPACE           TO  WRK-NAME
                   MOVE    TBL-SRYCD (IDX) TO  WRK-SRYCD
                   PERFORM     UNTIL   IDX >       WRK-KENSA-MAX
                               OR      TBL-SRYCD (IDX) (1:1)
                                           NOT =   WRK-SRYCD
      *                次の名称があるとき”、”＋名称をつける
                       IF      WRK-YUKOKETA    >   ZERO
                           STRING  WRK-NAME    DELIMITED  BY  SPACE
                                   "，"        DELIMITED  BY  SIZE
                                   INTO        WRK-NAME
                           END-STRING        
                           ADD     2           TO  WRK-YUKOKETA
      *              
                           COMPUTE IDZ     =   WRK-YUKOKETA        +
                                               TBL-YUKOKETA (IDX)  *  2
                           IF      IDZ             >   38
                               ADD     1               TO
                                                   WRK-ZAIWMAX (3)
                               MOVE    WRK-ZAIWMAX (3) TO  IDZ
                               MOVE    WRK-NAME        TO
                                           TBL-ZAIW-NAME(3 IDZ)
                               COMPUTE TBL-ZAIW-YUKOKETA (3 IDZ) 
                                           =   WRK-YUKOKETA   /   2  
                               MOVE    SPACE           TO  WRK-NAME
                               MOVE    ZERO            TO  WRK-YUKOKETA
                           END-IF
                       END-IF
      *
                       IF      TBL-YUKOKETA (IDX)  >   19
                           ADD     1               TO  WRK-ZAIWMAX (3)
                           MOVE    WRK-ZAIWMAX (3) TO  IDZ
                           MOVE    TBL-NAME (IDX) (1:38)   TO
                                       TBL-ZAIW-NAME    (3 IDZ)
                           MOVE    19                      TO  
                                       TBL-ZAIW-YUKOKETA(3 IDZ)
                           MOVE    TBL-NAME (IDX) (39:)    TO  WRK-NAME
                           COMPUTE WRK-YUKOKETA    =  
                                   ( TBL-YUKOKETA (IDX) -   19 )  *   2                                           
                       ELSE
                           STRING  WRK-NAME        DELIMITED  BY  SPACE
                                   TBL-NAME    (IDX)
                                                   DELIMITED  BY  SPACE
                                   INTO            WRK-NAME
                           END-STRING
                           COMPUTE WRK-YUKOKETA =  WRK-YUKOKETA   +
                                       TBL-YUKOKETA (IDX)  *   2
                       END-IF
      *                 
                       IF  TBL-DATAKBN (IDX)   NOT =   "2"
                          ADD  1           TO  WRK-KOUMOKUSU
                       END-IF
                       ADD     1           TO  IDX
                   END-PERFORM                            
                   ADD     1               TO  WRK-ZAIWMAX (3)
                   MOVE    WRK-ZAIWMAX (3) TO  IDZ
                   MOVE    WRK-NAME    TO  TBL-ZAIW-NAME (3 IDZ)
                   COMPUTE TBL-ZAIW-YUKOKETA (3 IDZ) 
                                       =   WRK-YUKOKETA   /   2
                   IF      (FLG-GAIJINKEN  =   1) 
                           PERFORM 200512-GJK-DAY-ADD-SEC
                   END-IF
               END-IF                                        
           END-PERFORM
      *
      *    検査の項目数をコメントにセット
           IF      WRK-TNS-HOUKNSKBN  >   ZERO
               INITIALIZE                 WRK-CHAR-HENKAN-AREA
               MOVE    1                  TO  WRK-CHAR-FLG
      *********MOVE    WRK-KENSA-MAX      TO  WRK-Z9
               MOVE    WRK-KOUMOKUSU      TO  WRK-Z9
               MOVE    WRK-Z9 (3:2)       TO  WRK-CHAR-X
               MOVE    2                  TO  WRK-CHAR-CNT
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               ADD     1                  TO  WRK-ZAIWMAX (2)
                                              WRK-ZAIWMAX (6)
               MOVE    WRK-ZAIWMAX (2)    TO  IDZ   
               IF      WRK-TNS-HOUKNSKBN  =   11
                   MOVE    WRK-CONS-KENSACOMMENT (WRK-TNS-HOUKNSKBN)
                                          TO    TBL-ZAIW-NAME (2 IDZ)
               ELSE
                   STRING  WRK-CONS-KENSACOMMENT (WRK-TNS-HOUKNSKBN)
                                              DELIMITED  BY  SPACE
                           "（"               DELIMITED  BY  SIZE
                           WRK-CHAR-N         DELIMITED  BY  SPACE
                           "項目）"           DELIMITED  BY  SIZE
                                          INTO    TBL-ZAIW-NAME (2 IDZ)
                   END-STRING
               END-IF
               PERFORM VARYING IDX FROM   300 BY  -1
                       UNTIL   IDX <      1
                   IF      TBL-ZAIW-NAME (2 IDZ) (IDX:1)
                                              NOT =   SPACE
                       COMPUTE TBL-ZAIW-YUKOKETA (2 IDZ)    =
                                                  IDX     /   2
                       MOVE    1          TO  IDX
                   END-IF
               END-PERFORM
               MOVE    "88888888"         TO  TBL-ZAIW-SRYCD(2 IDZ)
           END-IF                                    
      *
           COMPUTE WRK-ZAIWMAX (6)   =   WRK-ZAIWMAX  (6)     +   
                                         WRK-ZAIWMAX  (3) 
           MOVE    WRK-ZAIW-SRYCD1   TO  TBL-ZAIW-SRYCD (3 1) 
      *
           PERFORM 20052-SRYKBNELSE-CHECK-SEC
           .
       20051-SRYKBNELSE-KENSA-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査加算算定日編集処理
      *****************************************************************
       200511-GJK-DAY-ADD-SEC          SECTION.
      *
           INITIALIZE                  WRK-CHAR-HENKAN-AREA
      *
           PERFORM VARYING IDV FROM 1  BY  1
                   UNTIL   IDV >           31
                   IF  RECE44-KAISU (1 IDV)    >   0
                       MOVE    IDV     TO  WRK-Z9
                       MOVE    31      TO  IDV
                   END-IF
           END-PERFORM
      *
           MOVE    1                   TO  WRK-CHAR-FLG
           MOVE    WRK-Z9 (3:2)        TO  WRK-CHAR-X
           MOVE    2                   TO  WRK-CHAR-CNT
           MOVE    IDZ                 TO  IDV
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           MOVE    IDV                 TO  IDZ
      * 
           IF      WRK-Z9              <   10
                   MOVE    4           TO  IDV
           ELSE
                   MOVE    5           TO  IDV
           END-IF
           MOVE    SPACE               TO  WRK-NAME
           COMPUTE WRK-YUKOKETA        =   TBL-ZAIW-YUKOKETA(3 IDZ)
                                       +   IDV
           IF      WRK-YUKOKETA        >   19
                   ADD     1           TO  WRK-ZAIWMAX (3)
                   MOVE    WRK-ZAIWMAX (3) TO  IDZ
                   STRING  "（"               DELIMITED  BY  SIZE
                           WRK-CHAR-N         DELIMITED  BY  SPACE
                           "日）"             DELIMITED  BY  SIZE
                                       INTO   TBL-ZAIW-NAME (3 IDZ)
                   END-STRING
                   MOVE    IDV         TO     TBL-ZAIW-YUKOKETA(3 IDZ)
           ELSE
                   STRING  TBL-ZAIW-NAME (3 IDZ)
                                              DELIMITED  BY  SPACE
                           "（"               DELIMITED  BY  SIZE
                           WRK-CHAR-N         DELIMITED  BY  SPACE
                           "日）"             DELIMITED  BY  SIZE
                                       INTO   WRK-NAME
                   END-STRING
                   MOVE    WRK-NAME    TO  TBL-ZAIW-NAME (3 IDZ)
                   MOVE    WRK-YUKOKETA    TO  TBL-ZAIW-YUKOKETA(3 IDZ)
           END-IF
      *                 
           .
       200511-GJK-DAY-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査加算算定日編集処理
      *****************************************************************
       200512-GJK-DAY-ADD-SEC          SECTION.
      *
           INITIALIZE                  WRK-CHAR-HENKAN-AREA
      *
           PERFORM VARYING IDV FROM 1  BY  1
                   UNTIL   IDV >           31
                   IF  RECE44-KAISU (1 IDV)    >   0
                       MOVE    IDV     TO  WRK-Z9
                       MOVE    31      TO  IDV
                   END-IF
           END-PERFORM
      *
           MOVE    1                   TO  WRK-CHAR-FLG
           MOVE    WRK-Z9 (3:2)        TO  WRK-CHAR-X
           MOVE    2                   TO  WRK-CHAR-CNT
           MOVE    IDZ                 TO  IDV
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           MOVE    IDV                 TO  IDZ
      * 
           IF      WRK-Z9              <   10
                   MOVE    4           TO  IDV
           ELSE
                   MOVE    5           TO  IDV
           END-IF
           ADD     1                   TO  WRK-ZAIWMAX (3)
           MOVE    WRK-ZAIWMAX (3)     TO  IDZ
           STRING  "（"                DELIMITED  BY  SIZE
                   WRK-CHAR-N          DELIMITED  BY  SPACE
                   "日）"              DELIMITED  BY  SIZE
                                       INTO   TBL-ZAIW-NAME (3 IDZ)
           END-STRING
           MOVE    IDV                 TO  TBL-ZAIW-YUKOKETA(3 IDZ)
      *                 
           .
       200512-GJK-DAY-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブルチェック 診療区分（その他） 処理
      *****************************************************************
       20052-SRYKBNELSE-CHECK-SEC      SECTION.
      *
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    SPACE             TO  WRK-ZAI
           MOVE    "＊"              TO  WRK-ZAI-MK
      *     
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       5
      *
      *        薬剤と器材の表示を逆にする
               EVALUATE    IDX3
                   WHEN    4
                       MOVE    5               TO  IDX1
                   WHEN    5
                       MOVE    4               TO  IDX1
                   WHEN   OTHER
                       MOVE    IDX3            TO  IDX1
               END-EVALUATE
      *
               IF      RECE44-SRYKBN   <=   "33"
                   MOVE    "＊"            TO  WRK-ZAI-MK
               END-IF    
      *************IF      RECE44-SRYKBN     =   "31"  OR  "32"
      *************AND     IDX1              =   3
               IF    ((RECE44-YKZTEN           =   ZERO) AND
                      (RECE44-JIHOKBN          =  SPACE) AND
                      (RECE44-YKZKISAIFLG      =  SPACE) AND
                      (IDX1                    =   5   ))
               OR    ((RECE44-KIZAITEN         =   ZERO) AND
                      (RECE44-JIHOKBN          =  SPACE) AND
                      (IDX1                    =   4   ))
               OR    ((RECE44-HOKATU-YKZTEN    =   ZERO) AND
                      (RECE44-JIHOKBN      NOT =  SPACE) AND
                      (RECE44-YKZKISAIFLG      =  SPACE) AND
                      (IDX1                    =   5   ))
               OR    ((RECE44-HOKATU-KIZAITEN  =   ZERO) AND
                      (RECE44-JIHOKBN      NOT =  SPACE) AND
                      (IDX1                    =   4   ))
                   CONTINUE
               ELSE    
      ***          IF   (RECE44-SRYKBN     =   "31" OR "32")
      ***          AND  (FLG-HIKAKIN       =    1          )
      ***          AND  (IDX1              =    3          )
      ***              CONTINUE
      ***          ELSE    
                       IF  ( RECE44-SRYKBN        =   "31" OR "32" )
                       OR  ((RECE44-SRYKBN        =   "33"        ) AND
                            (RECE44-JIHOKBN   NOT =   SPACE       ))
                         IF   WRK-TBLMAX      >    ZERO
                             MOVE     SPACE      TO  WRK-ZAI-MK
                         END-IF
                       END-IF
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL   IDX2    >   WRK-ZAIWMAX (IDX1)  
                           MOVE    ZERO          TO  FLG-NOPRT
                           MOVE    IDX1          TO  WRK-WIDX1
                           MOVE    IDX2          TO  WRK-WIDX2
                           PERFORM 200521-SEYKBNELSE-MEISAI-SEC
                           IF  FLG-NOPRT         =   ZERO
      *                        レセプト０５剤編集処理
                               PERFORM 430-TBLZAI-HEN-SEC
                           END-IF    
                       END-PERFORM
      ***          END-IF
               END-IF     
               IF      WRK-ZAIWMAX (IDX1)    >   ZERO        
                   MOVE    SPACE             TO  WRK-ZAI
               END-IF    
      *             
      *        点数・回数セット処理（診療行為）
               IF      IDX1              =   3
               AND     WRK-ZAIWMAX(IDX1) >   ZERO
               AND     RECE44-JIHOKBN    =   SPACE
                   IF      RECE44-SRYKBN     >=  "33"       
                       MOVE    RECE44-SYUTEN1    TO  WRK-ZAITENX
                       MOVE    RECE44-ZAIKAISU   TO  WRK-ZAIKAISUX
                       PERFORM 810-TENSUKAI-HEN-SEC
                       MOVE    WRK-TBLMAX        TO  IDY
                       PERFORM 440-TNSKAI-SET-SEC
                       MOVE    IDY               TO  WRK-TBLMAX 
                   END-IF
               END-IF    
      *        点数・回数セット処理（特定器材）
               IF     ( IDX1                =   4    )
               AND    ( WRK-ZAIWMAX(IDX1)   >   ZERO )
               AND    ( RECE44-JIHOKBN      =   SPACE)
                 IF     RECE44-KIZAITEN     >   ZERO
                   IF      RECE44-SRYKBN     >=  "33"   
                       MOVE    RECE44-KIZAITEN   TO  WRK-ZAITENX
                       MOVE    RECE44-ZAIKAISU   TO  WRK-ZAIKAISUX
                       PERFORM 810-TENSUKAI-HEN-SEC
                       MOVE    WRK-TBLMAX        TO  IDY
                       PERFORM 440-TNSKAI-SET-SEC
                       MOVE    IDY               TO  WRK-TBLMAX 
                   END-IF
                 END-IF
               END-IF     
      *        点数・回数セット処理（薬剤）
               IF     ( IDX1                =   5    )
               AND    ( WRK-ZAIWMAX(IDX1)   >   ZERO )
               AND    ( RECE44-JIHOKBN      =   SPACE)
                 IF     RECE44-YKZTEN       >   ZERO
                 OR     RECE44-YKZKISAIFLG  =   "1"
      ***          IF      RECE44-SRYKBN     >=  "31"   
      *                点数・回数セット
                       IF      RECE44-SRYKBN     >=   "33"
                           MOVE    RECE44-YKZTEN     TO  WRK-ZAITENX
                           MOVE    RECE44-ZAIKAISU   TO  WRK-ZAIKAISUX
                           PERFORM 810-TENSUKAI-HEN-SEC
                           MOVE    WRK-TBLMAX        TO  IDY
                           PERFORM 440-TNSKAI-SET-SEC
                           MOVE    IDY               TO  WRK-TBLMAX 
                       END-IF          
      ***          END-IF
                 END-IF
               END-IF        
           END-PERFORM
      *             
      *    検査のとき回数を集計
           IF     (WRK-ZAIWMAX(3)     >   ZERO        )
           AND    (RECE44-SRYKBN      =   "60" OR "64")
           AND    (RECE44-JIHOKBN     =   SPACE       )
               IF      TBL-ZAIW-SRYCD (3 1)   =   WRK-CONS-SAIKETSU1
                                              OR  WRK-CONS-SAIKETSU2
                                              OR  WRK-CONS-SAIKETSU3 
                   CONTINUE
               ELSE
                   IF      WRK-ZAIW-KNSJISKBN  =  "2"
                       CONTINUE
                   ELSE 
                       ADD     RECE44-ZAIKAISU   TO  WRK-KENSA-KAISU
                   END-IF
               END-IF
           END-IF     
      *
           .
       20052-SRYKBNELSE-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（その他） 処理
      *****************************************************************
       200521-SEYKBNELSE-MEISAI-SEC        SECTION.
      *
           MOVE    ZERO          TO  FLG-NOPRT
      *
      *    服用方法でないこと
           IF  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3) =   "001")
           OR  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:1) =   "S"  )
               MOVE    1             TO  FLG-NOPRT
           END-IF
      *    診療行為テーブルに存在した場合、印字しないこと
           IF   RECE44-JIHOKBN  =  SPACE
             PERFORM   VARYING   IDY   FROM    1   BY  1
                       UNTIL   ( IDY           >   TBL-NOSRYCD-MAX )
                       OR      ( FLG-NOPRT     =    1 )
      *
               IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                                =   TBL-NOSRYCD (IDY)
                   MOVE    1                 TO  FLG-NOPRT
               END-IF
      *
      *        データ区分２で、診療区分が点数マスタの診療区分と異なる場合
               IF  (TBL-ZAIW-DATAKBN(WRK-WIDX1 WRK-WIDX2)  =  2 )
               AND (RECE44-SRYKBN  NOT =
                         TBL-ZAIW-TNSSRYKBN(WRK-WIDX1 WRK-WIDX2))
                   MOVE    ZERO              TO  FLG-NOPRT
               END-IF
             END-PERFORM
           END-IF
      *
      ***  IF   ( RECE44-SRYKBN               = "31" OR "32" )
      ***  AND  ( TBL-ZAIW-NAME (3 1)         = SPACE )
      ***  AND  ( TBL-ZAIW-NAME (3 WRK-WIDX2) = SPACE )
      ***  AND  ( WRK-WIDX1                   = 3     )
      ***  AND  ( WRK-WIDX2               NOT = 1     )
      ***          MOVE    1                 TO  FLG-NOPRT
      ***  END-IF
           IF    (RECE44-SRYKBN    =   "31" OR "32")
           AND   (RECE44-JIHOKBN   =   SPACE       )
              IF  FLG-HIKAKIN     =  ZERO
                IF   ( TBL-ZAIW-NAME (3 1)         = SPACE )
                AND  ( TBL-ZAIW-NAME (3 2)     NOT = SPACE )
                AND  ( WRK-WIDX1                   = 3     )
                AND  ( WRK-WIDX2                   = 1     )
                   MOVE    1                 TO  FLG-NOPRT
                END-IF
              ELSE
                IF   ( TBL-ZAIW-NAME (3 1)         = SPACE )
                AND  ( WRK-WIDX1                   = 3     )
                AND  ( WRK-WIDX2                   = 1     )
                   MOVE    1                 TO  FLG-NOPRT
                END-IF
              END-IF
           END-IF
      *
           .
       200521-SEYKBNELSE-MEISAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分（その他）　例外処理
      *****************************************************************
       20042-SRYKBNELSE-REIGAI-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
           MOVE    "＊"                TO  WRK-ZAI-MK
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       5  
      *
      *        薬剤と器材の表示を逆にする
               EVALUATE    IDX3
                   WHEN    4
                       MOVE    5               TO  IDX1
                   WHEN    5
                       MOVE    4               TO  IDX1
                   WHEN   OTHER
                       MOVE    IDX3            TO  IDX1
               END-EVALUATE
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >   WRK-ZAIWMAX (IDX1)  
                   MOVE    IDX1          TO  WRK-WIDX1
                   MOVE    IDX2          TO  WRK-WIDX2
      *            レセプト０５剤編集処理
                   PERFORM 430-TBLZAI-HEN-SEC
      ****         ADD     TBL-ZAIW-TEN (IDX IDX1) TO  WRK-TENSUGOKEI
               END-PERFORM    
           END-PERFORM
      *
           .
       20042-SRYKBNELSE-REIGAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査チェック処理
      *****************************************************************
       2006-SRYKBN60-CHECK-SEC         SECTION.
      *
           PERFORM 2006-GAIJINKEN-BUNRI-SEC
           MOVE    RECE44-REC          TO   RECE44SAV-REC
      *
      *    薬剤やコメントがあればまとめない
           IF      FLG-SRYCD1          =   1
               PERFORM 2006-GAIJINKEN-HENSYU-SEC
               MOVE    RECE44SAV-REC       TO  RECE44-REC
               IF      RECE44-ZAIKAISU     >   0
                       MOVE    ZERO        TO  FLG-GAIJINKEN
                       COMPUTE RECE44-ZAINUM   =   RECE44-ZAINUM
                                               +   99000000
                       PERFORM 2005-SRYKBNELSE-HENSYU-SEC
               END-IF
           ELSE
      *        剤の明細数が３以上の場合と明細数が２で
      *        先頭が検体名コードでない場合はまとめない
               IF    ( RECE44-MEISAISU     >   2 )
                 OR (( RECE44-MEISAISU     =   2 )
                 AND ( TBL-ZAIW-KENTAICOMMENT-SRYCD (3 1) = 0 ))
                   PERFORM 2006-GAIJINKEN-HENSYU-SEC
                   MOVE    RECE44SAV-REC       TO  RECE44-REC
                   IF      RECE44-ZAIKAISU     >   0
                           MOVE    ZERO        TO  FLG-GAIJINKEN
                           COMPUTE RECE44-ZAINUM   =   RECE44-ZAINUM
                                                   +   99000000
                           PERFORM 2005-SRYKBNELSE-HENSYU-SEC
                   END-IF
               ELSE    
      *            包括対象でないときまとめない         
                   IF      TBL-ZAIW-HOUKNSKBN(3 1)  =   ZERO
                       PERFORM 2006-GAIJINKEN-HENSYU-SEC
                       MOVE    RECE44SAV-REC       TO  RECE44-REC
                       IF      RECE44-ZAIKAISU     >   0
                               MOVE    ZERO        TO  FLG-GAIJINKEN
                               COMPUTE RECE44-ZAINUM   =   RECE44-ZAINUM
                                                       +   99000000
                               PERFORM 2005-SRYKBNELSE-HENSYU-SEC
                       END-IF
                   ELSE
                       IF      WRK-RECE44GJK-MAX   >   0
                               PERFORM VARYING IDXX FROM  1  BY  1 
                                       UNTIL   IDXX >  WRK-RECE44GJK-MAX 
                                 MOVE  1           TO  FLG-GAIJINKEN
                                 MOVE  WRK-RECE44GJK-REC (IDXX) 
                                                   TO  RECE44-REC 
                                 PERFORM 2006-SRYKBN60-TAIHI-SEC
                               END-PERFORM
                       END-IF
                       MOVE    RECE44SAV-REC       TO  RECE44-REC
                       IF      RECE44-ZAIKAISU     >   0
                               MOVE    ZERO        TO  FLG-GAIJINKEN
                               PERFORM 2006-SRYKBN60-TAIHI-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       2006-SRYKBN60-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査加算剤分離処理
      *****************************************************************
       2006-GAIJINKEN-BUNRI-SEC        SECTION.
      *
           INITIALIZE                  WRK-RECE44GJK-TABLE
           MOVE    ZERO                TO  WRK-RECE44GJK-MAX
      *
           IF      ( RECE41-KENTAIKASANFLG   =   '1' )
             AND   ( WRK-ZAIW-KNSJISKBN  NOT =   2 )
             AND  (((RECE44-KNSJISGRPKBN     >   0)
             AND    (RECE44-KNSJISGRPKBN     <   7))
             OR     (RECE44-SRYCDTOTAL       =   WRK-CONS-NYOIPPAN-N))
             MOVE    RECE44-REC        TO  RECE44GJK-REC
             PERFORM VARYING IDX FROM  1   BY  1
                     UNTIL   IDX >     31
                 IF  RECE44-KAISU (1 IDX)    >   0
                   IF  WRK-KENTAIKASAN (IDX) =   1
                       MOVE    ZERO    TO  RECE44GJK-ZAIKAISU
                                           RECE44GJK-DAY-AREA
                       MOVE    RECE44-KAISU (1 IDX)
                                       TO  RECE44GJK-ZAIKAISU
                       PERFORM VARYING IDX7 FROM  1  BY  1
                                UNTIL  IDX7   >  10
                         MOVE    RECE44-KAISU(IDX7 IDX)
                                       TO  RECE44GJK-KAISU(IDX7 IDX)
                       END-PERFORM
                       COMPUTE WRK-RECE44GJK-MAX =  WRK-RECE44GJK-MAX
                                                 +  1
                       MOVE    RECE44GJK-REC       
                                       TO  WRK-RECE44GJK-REC 
                                               (WRK-RECE44GJK-MAX)
                       COMPUTE RECE44-ZAIKAISU   =  RECE44-ZAIKAISU
                                       -   RECE44-KAISU (1 IDX)
                       PERFORM VARYING IDX7 FROM  1  BY  1
                                UNTIL  IDX7   >  10
                         MOVE    ZERO  TO  RECE44-KAISU(IDX7 IDX)
                       END-PERFORM
                   END-IF
                 END-IF
             END-PERFORM
           END-IF        
           .
       2006-GAIJINKEN-BUNRI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査加算対象剤処理
      *****************************************************************
       2006-GAIJINKEN-HENSYU-SEC       SECTION.
      *
           IF     WRK-RECE44GJK-MAX    >   0
                  PERFORM VARYING IDXX FROM  1  BY  1 
                          UNTIL   IDXX >     WRK-RECE44GJK-MAX 
                      MOVE  WRK-RECE44GJK-REC (IDXX) 
                                       TO  RECE44-REC 
                      COMPUTE WRK-GAIJINKEN-ZAINUM  =
                              WRK-GAIJINKEN-ZAINUM  +  1
                      COMPUTE RECE44-ZAINUM = RECE44-ZAINUM
                                            + WRK-GAIJINKEN-ZAINUM
                                            * 100000
                      MOVE    1        TO  FLG-GAIJINKEN
                      MOVE    TBL-ZAIW-OCC (3)
                                       TO  TBL-ZAIW-OCC (6)
                      MOVE    WRK-ZAIWMAX-G
                                       TO  WRK-ZAIWMAX-SAV-G
                      PERFORM 2005-SRYKBNELSE-HENSYU-SEC
                      MOVE    TBL-ZAIW-OCC (6)
                                       TO  TBL-ZAIW-OCC (3)
                      MOVE    WRK-ZAIWMAX-SAV-G
                                       TO  WRK-ZAIWMAX-G
                  END-PERFORM
           END-IF
           .
       2006-GAIJINKEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査まとめ対象待避処理
      *****************************************************************
       2006-SRYKBN60-TAIHI-SEC         SECTION.
      *
           ADD     1               TO  WRK-RECE44-MAX
           MOVE    RECE44-REC      TO  WRK-RECE44-REC (WRK-RECE44-MAX)
           MOVE    RECE44-DAY-AREA TO  WRK-RECE44-DAY-AREA
                                                      (WRK-RECE44-MAX)
           MOVE    TBL-ZAIW-HOUKNSKBN(3 1) TO  WRK-RECE44-HOUKNSKBN
                                                      (WRK-RECE44-MAX)
           IF      FLG-GAIJINKEN   =   0                    
                   MOVE ZERO       TO  WRK-RECE44-GAIJINKEN
                                                      (WRK-RECE44-MAX)
           ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       IF  RECE44-KAISU (1 IDX)    >   0
                           MOVE IDX TO  WRK-RECE44-GAIJINKEN
                                                      (WRK-RECE44-MAX)
                           MOVE 31  TO  IDX
                       END-IF
                   END-PERFORM
           END-IF
           MOVE    TBL-ZAIW-SRYSYUKBN-INPUT(3 1)
                                   TO  WRK-RECE44-SRYSYUKBN
                                                      (WRK-RECE44-MAX)
           MOVE    TBL-ZAIW-SRYCD(3 1)
                                   TO  WRK-RECE44-SRYCD
                                                      (WRK-RECE44-MAX)
           MOVE    TBL-ZAIW-SRYSURYO(3 1)
                                   TO  WRK-RECE44-SRYSURYO
                                                      (WRK-RECE44-MAX)
           MOVE    TBL-ZAIW-KENTAICOMMENT-SRYCD (3 1)
                                   TO  WRK-RECE44-KENTAICOMMENT-SRYCD
                                                      (WRK-RECE44-MAX)
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               ADD      RECE44-KAISU (1 IDX)   TO  WRK-SUM-KAISU (IDX)
               IF      (RECE44-KAISU (1 IDX)   >   0)
                 IF    (WRK-MIN-KAISU (IDX)    =   0)
                 OR    (RECE44-KAISU (1 IDX)   <   WRK-MIN-KAISU (IDX))
                   MOVE RECE44-KAISU (1 IDX)   TO  WRK-MIN-KAISU (IDX)
                   MOVE TBL-ZAIW-HOUKNSKBN(3 1)
                                            TO  WRK-MIN-HOUKNSKBN (IDX)
                   MOVE WRK-RECE44-GAIJINKEN (WRK-RECE44-MAX)
                                            TO  WRK-MIN-GAIJINKEN (IDX)
                   MOVE TBL-ZAIW-SRYSYUKBN-INPUT(3 1)
                                            TO  WRK-MIN-SRYSYUKBN (IDX)
                 END-IF
               END-IF
           END-PERFORM                                                       
           .
       2006-SRYKBN60-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括対象検査まとめ処理
      *****************************************************************
       2007-SRYKBN60-HENSYU-SEC         SECTION.
      * 
           INITIALIZE                   WRK-RECE44-MATOME-TABLE
      *
           PERFORM     VARYING  IDXX     FROM  1  BY  1
                       UNTIL    IDXX     >    31
      *
             PERFORM            UNTIL   WRK-SUM-KAISU (IDXX)  =  ZERO
               INITIALIZE       WRK-KENSA-MATOME (101)
                                WRK-RECE44-FLG-TABLE
               PERFORM  VARYING IDY FROM    1   BY  1
                        UNTIL   IDY >       WRK-RECE44-MAX
                 IF    ( WRK-RECE44-KAISU (IDY 1 IDXX)
                                       >=   WRK-MIN-KAISU (IDXX) )
                 AND   ( WRK-RECE44-HOUKNSKBN (IDY)
                                        =   WRK-MIN-HOUKNSKBN (IDXX) )
                 AND   ( WRK-RECE44-GAIJINKEN (IDY)
                                        =   WRK-MIN-GAIJINKEN (IDXX) )
                 AND   ( WRK-RECE44-SRYSYUKBN (IDY)
                                        =   WRK-MIN-SRYSYUKBN (IDXX) )
                 AND   ( WRK-RECE44-FLG (IDY)      =   ZERO )
                    MOVE  WRK-RECE44-HOUKNSKBN (IDY)
                                              TO  WRK-HOUKNSKBN (101)
                    MOVE  WRK-RECE44-GAIJINKEN (IDY)
                                              TO  WRK-GAIJINKEN (101)
                    MOVE  WRK-RECE44-SRYSYUKBN (IDY)
                                              TO  WRK-SRYSYUKBN-K (101)
                    MOVE  WRK-MIN-KAISU (IDXX)
                                              TO  WRK-KAISU (101 1)
                    MOVE  1                   TO  WRK-RECE44-FLG (IDY)
                    ADD   1                   TO  WRK-KENSA-SU (101)
                    MOVE  WRK-KENSA-SU (101)  TO  IDX1
                    MOVE  IDY             TO  WRK-RECE44-IDX (101 IDX1)
                    PERFORM  VARYING  IDX2  FROM 1 BY 1
                             UNTIL    IDX2  >    IDX1
                      IF   WRK-KENSA-SRYCD(101 IDX2)  =  0
                           MOVE  WRK-RECE44-SRYCD-G (IDY)
                                       TO  WRK-KENSA-SRYCD-OCC(101 IDX2)
                      ELSE
                        IF   WRK-RECE44-SRYCD-G (IDY)
                                       <   WRK-KENSA-SRYCD-OCC(101 IDX2)
                             PERFORM  VARYING  IDX3  FROM  IDX1  BY -1
                                      UNTIL    IDX3  =     IDX2
                               COMPUTE  IDX4   =     IDX3  -  1
                               MOVE   WRK-KENSA-SRYCD-OCC(101 IDX4)
                                       TO  WRK-KENSA-SRYCD-OCC(101 IDX3)
                             END-PERFORM
                             MOVE  WRK-RECE44-SRYCD-G (IDY)
                                       TO  WRK-KENSA-SRYCD-OCC(101 IDX2)
                             MOVE  IDX1   TO  IDX2
                        END-IF
                      END-IF
                    END-PERFORM
                 END-IF
               END-PERFORM
      *
      *        同一検査内容のグループがテーブルに存在するか
               MOVE    ZERO                   TO  IDX1
               PERFORM  VARYING IDY FROM    1   BY  1
                        UNTIL   IDY >       WRK-KENSA-MATOME-MAX
                 IF    ( WRK-HOUKNSKBN (IDY)  =  WRK-HOUKNSKBN (101) )
                 AND   ( WRK-GAIJINKEN (IDY)  =  WRK-GAIJINKEN (101) )
                 AND   ( WRK-KENSA-SU (IDY)   =  WRK-KENSA-SU (101) )
                   IF  ( WRK-RECE44-IDX-G (IDY)
                                           =  WRK-RECE44-IDX-G (101) )
                   OR  ( WRK-KENSA-SRYCD-G (IDY)
                                           =  WRK-KENSA-SRYCD-G (101) )
                    MOVE  IDY                 TO  IDX1
                    MOVE  WRK-KENSA-MATOME-MAX
                                              TO  IDY
                   END-IF
                 END-IF
               END-PERFORM
      *
               IF      IDX1                   =   0
                       ADD  1                 TO  WRK-KENSA-MATOME-MAX
                       MOVE WRK-KENSA-MATOME (101)
                            TO  WRK-KENSA-MATOME (WRK-KENSA-MATOME-MAX)
               ELSE
                       ADD  WRK-MIN-KAISU (IDXX)
                                              TO  WRK-KAISU (IDX1 1)
               END-IF
      *
      *        回数テーブルより差し引く
               PERFORM  VARYING IDY FROM    1   BY  1
                        UNTIL   IDY >       WRK-RECE44-MAX
                 IF    ( WRK-RECE44-FLG (IDY)      =   1 )
                    COMPUTE WRK-SUM-KAISU (IDXX)   =
                            WRK-SUM-KAISU (IDXX)   -
                            WRK-MIN-KAISU (IDXX)
                    COMPUTE WRK-RECE44-KAISU (IDY 1 IDXX)   =
                            WRK-RECE44-KAISU (IDY 1 IDXX)   -
                            WRK-MIN-KAISU (IDXX)
                 END-IF
               END-PERFORM
      *
      *        新たに最小回数情報をセットする
               MOVE     ZERO             TO  WRK-MIN-KAISU (IDXX)
               MOVE     ZERO             TO  WRK-MIN-HOUKNSKBN (IDXX)
               MOVE     ZERO             TO  WRK-MIN-GAIJINKEN (IDXX)
               MOVE     SPACE            TO  WRK-MIN-SRYSYUKBN (IDXX)
               PERFORM  VARYING IDY FROM    1   BY  1
                        UNTIL   IDY >       WRK-RECE44-MAX
                 IF    (WRK-RECE44-KAISU (IDY 1 IDXX)  >   0)
                   IF  (WRK-MIN-KAISU (IDXX)   =   0)
                   OR  (WRK-RECE44-KAISU (IDY 1 IDXX)
                                             <   WRK-MIN-KAISU (IDXX))
                     MOVE WRK-RECE44-KAISU (IDY 1 IDXX)
                                           TO  WRK-MIN-KAISU (IDXX)
                     MOVE WRK-RECE44-HOUKNSKBN (IDY)
                                           TO  WRK-MIN-HOUKNSKBN (IDXX)
                     MOVE WRK-RECE44-GAIJINKEN (IDY)
                                           TO  WRK-MIN-GAIJINKEN (IDXX)
                     MOVE WRK-RECE44-SRYSYUKBN (IDY)
                                           TO  WRK-MIN-SRYSYUKBN (IDXX)
                   END-IF
                 END-IF
               END-PERFORM
             END-PERFORM
           END-PERFORM
      *
      *    まとめた剤を出力する
           PERFORM      VARYING IDXX FROM   1   BY  1
                        UNTIL   IDXX >      WRK-KENSA-MATOME-MAX
             MOVE    ZERO                TO  FLG-SRYCD1
      *
             INITIALIZE                      TBL-ZAIW-AREA
             MOVE    ZERO                TO  WRK-ZAIWMAX-G
             MOVE    ZERO                TO  WRK-TBLMAX
             MOVE    ZERO                TO  FLG-TEIGEN
                                             FLG-COMMENT
                                             FLG-COMMENT2
                                             FLG-COMMENT3
                                             FLG-HIKAKIN 
             MOVE    ZERO                TO  WRK-RECE45
             MOVE    SPACE               TO  WRK-SRYSYUKBN
      *
             MOVE    SPACE               TO  RECE44-REC
             INITIALIZE                      RECE44-REC
             MOVE    ZERO                TO  WRK-RECE44-ZAITEN
             PERFORM VARYING IDW FROM    1   BY  1
                     UNTIL   IDW >       WRK-KENSA-SU (IDXX)
               MOVE  WRK-RECE44-IDX (IDXX IDW) TO  IDX1
               MOVE  WRK-RECE44-REC (IDX1)     TO  RECE44-REC
               MOVE  WRK-RECE44-DAY-AREA(IDX1) TO  RECE44-DAY-AREA
               ADD   RECE44-ZAITEN             TO  WRK-RECE44-ZAITEN
               PERFORM 2002-TBLZAIW-TAIHI-SEC
             END-PERFORM    
             MOVE    WRK-KAISU (IDXX 1)  TO  RECE44-ZAIKAISU
             MOVE    WRK-RECE44-ZAITEN   TO  RECE44-SYUTEN1
                                             RECE44-ZAITEN
             IF      WRK-GAIJINKEN (IDXX)  >  0
                     COMPUTE WRK-GAIJINKEN-ZAINUM  =
                             WRK-GAIJINKEN-ZAINUM  +  1
                     COMPUTE RECE44-ZAINUM = RECE44-ZAINUM
                                           + WRK-GAIJINKEN-ZAINUM
                                           * 100000
                     MOVE    1           TO  FLG-GAIJINKEN
             ELSE
                    COMPUTE RECE44-ZAINUM = RECE44-ZAINUM
                                          + 99000000
                     MOVE    0           TO  FLG-GAIJINKEN
             END-IF
      *
      *     レセプト０４剤編集前処理
            PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *
            IF      SPA-STS          =   ZERO
                    PERFORM 20051-SRYKBNELSE-KENSA-SEC
      *             剤・診療行為テーブル出力処理
                    PERFORM 450-RECE46-OUT-SEC
            END-IF
           END-PERFORM
           .
       2007-SRYKBN60-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査加算の算定日確認処理
      *****************************************************************
       2009-KENTAIKASAN-KAKUNIN-SEC      SECTION.
      *
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                   )
               OR     ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
      ***      IF     RECE44-SRYCDTOTAL      =   WRK-CONS-GAIJINKEN-N
               IF     RECE44-KNSJISGRPKBN    =   99
      *
                   PERFORM VARYING IDX FROM      1  BY  1
                           UNTIL   IDX >         31
                       IF  RECE44-KAISU (1 IDX)  >    0
                           MOVE    1         TO  WRK-KENTAIKASAN (IDX)
                       END-IF
                   END-PERFORM
               END-IF
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-RECE44
           .
      *
       2009-KENTAIKASAN-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診の剤まとめ編集
      *****************************************************************
       2010-SYOSAIMATOME-HENSYU1-SEC      SECTION.
      *
           IF  TBL-SORT11-ZAIKAISU(IDX6)  NOT =  ZERO
      *
               EVALUATE    TRUE
                 WHEN    TBL-SORT11-YUKOKETA(IDX6) <=  16
                   MOVE    SPACE             TO  WRK-ZAI
                   MOVE    TBL-SORT11-TEN(IDX6)  TO  WRK-ZAITENX
                   MOVE    TBL-SORT11-ZAIKAISU(IDX6)
                                                 TO  WRK-ZAIKAISUX
                   PERFORM 810-TENSUKAI-HEN-SEC
                   MOVE    WRK-TNSKAI        TO  WRK-ZAI-TNSKAI
      *
                   MOVE    "＊"              TO  WRK-ZAI-MK
                   MOVE    TBL-SORT11-YUKOKETA(IDX6)
                                             TO  IDX8
                   COMPUTE IDX8 = IDX8 * 2
                   MOVE    TBL-SORT11-NAME(IDX6)(1:IDX8)
                                             TO  WRK-ZAI-NAME(1:IDX8)
      *
                   MOVE    1                 TO  WRK-SET-LINENUM
                   PERFORM 20101-SYOSAIMATOME-WRITE1-SEC
                 WHEN    TBL-SORT11-YUKOKETA(IDX6) <=  41
                   MOVE    SPACE             TO  WRK-ZAI
                   MOVE    "＊"              TO  WRK-ZAI-MK
                   MOVE    TBL-SORT11-YUKOKETA(IDX6)
                                             TO  IDX8
                   COMPUTE IDX8 = IDX8 * 2
                   IF   IDX8  <=  50
                     MOVE  TBL-SORT11-NAME(IDX6)(1:IDX8)
                                             TO  WRK-ZAI-NAME(1:IDX8)
                   ELSE
                     MOVE  TBL-SORT11-NAME(IDX6)(1:50)
                                             TO  WRK-ZAI-NAME(1:50)
                   END-IF
      *
                   MOVE    1                 TO  WRK-SET-LINENUM
                   PERFORM 20101-SYOSAIMATOME-WRITE1-SEC
      *@
                   MOVE    SPACE             TO  WRK-ZAI
                   MOVE    TBL-SORT11-TEN(IDX6)  TO  WRK-ZAITENX
                   MOVE    TBL-SORT11-ZAIKAISU(IDX6)
                                                 TO  WRK-ZAIKAISUX
                   PERFORM 810-TENSUKAI-HEN-SEC
                   MOVE    WRK-TNSKAI        TO  WRK-ZAI-TNSKAI
                   IF   IDX8  >  50
                     COMPUTE IDX9 = IDX8 - 50
                     MOVE    TBL-SORT11-NAME(IDX6)(51:IDX9)
                                             TO  WRK-ZAI-NAME(1:IDX9)
                   END-IF
      *
                   MOVE    2                 TO  WRK-SET-LINENUM
                   PERFORM 20101-SYOSAIMATOME-WRITE1-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
      *
       2010-SYOSAIMATOME-HENSYU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診の剤まとめ出力
      *****************************************************************
       20101-SYOSAIMATOME-WRITE1-SEC      SECTION.
      *
           MOVE    SPACE             TO  RECE46-REC
           INITIALIZE                    RECE46-REC
           MOVE    RECE41-KEY        TO  RECE46-KEY
      *
           MOVE    WRK-SET-LINENUM   TO  RECE46-LINENUM
      *
           EVALUATE  IDX6
             WHEN     1
               COMPUTE RECE46-ZAINUM   = 99992001
             WHEN     2
               COMPUTE RECE46-ZAINUM   = 99992011
             WHEN     3
               COMPUTE RECE46-ZAINUM   = 99992021
             WHEN     4
               COMPUTE RECE46-ZAINUM   = 99992031
           END-EVALUATE
      *
           MOVE    1                 TO  RECE46-ZAIMEISAISU
           MOVE    RECE41-PTNUM      TO  RECE46-PTNUM
           MOVE    SPACE             TO  RECE46-SRYKA
           MOVE    WRK-ZAI           TO  RECE46-TEKIYO-INF
      *
           MOVE    "11"              TO  RECE46-SRYKBN
      *
           MOVE    99999999          TO  RECE46-ZAITEN
           MOVE    ZERO              TO  RECE46-KNSJISKBN
      *
           IF      IDX6  =  1
             MOVE    9               TO  RECE46-CDKBN-SYO
             MOVE    99              TO  RECE46-CDKBN-BU
             MOVE    "9"             TO  RECE46-CDKBN-KBN
             MOVE    999             TO  RECE46-CDKBN-KBNNUM
             MOVE    99              TO  RECE46-CDKBN-KBNNUM-EDA
             MOVE    99              TO  RECE46-CDKBN-KOUBAN
             MOVE    999999999       TO  RECE46-KOUHYOJUNNUM
           END-IF
      *
           MOVE    SPACE             TO  RECE46-SAFLG
           MOVE    ZERO              TO  RECE46-ZAIKBN
           MOVE    SPACE             TO  RECE46-SRYSYUKBN
           MOVE    ZERO              TO  RECE46-DAY
      *
           MOVE    SPACE             TO  RECE46-JIHOKBN
      *
           WRITE   RECE46-REC
      *
           IF  STS-RECE46        NOT =   ZERO
               DISPLAY "*** ORCR0460 RECE46 WRITE ERR  ***"
               DISPLAY "*** RECE46-SRYKBN ***" RECE46-SRYKBN
               DISPLAY "*** RECE46-ZAINUM ***" RECE46-ZAINUM
               DISPLAY "<SYOSAIMATOME-WRITE1>"
               MOVE    2             TO  SPA-STS
           ELSE
               ADD     1             TO  CNT-RECE46
           END-IF
      *
           .
      *
       20101-SYOSAIMATOME-WRITE1-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診の剤まとめ編集
      *****************************************************************
       2010-SYOSAIMATOME-HENSYU2-SEC      SECTION.
      *
           IF  TBL-SORT12-ZAIKAISU(IDX6)  NOT =  ZERO
      *
               EVALUATE    TRUE
                 WHEN    TBL-SORT12-YUKOKETA(IDX6) <=  16
                   MOVE    SPACE             TO  WRK-ZAI
                   MOVE    TBL-SORT12-TEN(IDX6)  TO  WRK-ZAITENX
                   MOVE    TBL-SORT12-ZAIKAISU(IDX6)
                                                 TO  WRK-ZAIKAISUX
                   PERFORM 810-TENSUKAI-HEN-SEC
                   MOVE    WRK-TNSKAI        TO  WRK-ZAI-TNSKAI
      *
                   MOVE    "＊"              TO  WRK-ZAI-MK
                   MOVE    TBL-SORT12-YUKOKETA(IDX6)
                                             TO  IDX8
                   COMPUTE IDX8 = IDX8 * 2
                   MOVE    TBL-SORT12-NAME(IDX6)(1:IDX8)
                                             TO  WRK-ZAI-NAME(1:IDX8)
      *
                   MOVE    1                 TO  WRK-SET-LINENUM
                   PERFORM 20101-SYOSAIMATOME-WRITE2-SEC
                 WHEN    TBL-SORT12-YUKOKETA(IDX6) <=  41
                   MOVE    SPACE             TO  WRK-ZAI
                   MOVE    "＊"              TO  WRK-ZAI-MK
                   MOVE    TBL-SORT12-YUKOKETA(IDX6)
                                             TO  IDX8
                   COMPUTE IDX8 = IDX8 * 2
                   IF   IDX8  <=  50
                     MOVE  TBL-SORT12-NAME(IDX6)(1:IDX8)
                                             TO  WRK-ZAI-NAME(1:IDX8)
                   ELSE
                     MOVE  TBL-SORT12-NAME(IDX6)(1:50)
                                             TO  WRK-ZAI-NAME(1:50)
                   END-IF
      *
                   MOVE    1                 TO  WRK-SET-LINENUM
                   PERFORM 20101-SYOSAIMATOME-WRITE2-SEC
      *@
                   MOVE    SPACE             TO  WRK-ZAI
                   MOVE    TBL-SORT12-TEN(IDX6)  TO  WRK-ZAITENX
                   MOVE    TBL-SORT12-ZAIKAISU(IDX6)
                                                 TO  WRK-ZAIKAISUX
                   PERFORM 810-TENSUKAI-HEN-SEC
                   MOVE    WRK-TNSKAI        TO  WRK-ZAI-TNSKAI
                   IF   IDX8  >  50
                     COMPUTE IDX9 = IDX8 - 50
                     MOVE    TBL-SORT12-NAME(IDX6)(51:IDX9)
                                             TO  WRK-ZAI-NAME(1:IDX9)
                   END-IF
      *
                   MOVE    2                 TO  WRK-SET-LINENUM
                   PERFORM 20101-SYOSAIMATOME-WRITE2-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
      *
       2010-SYOSAIMATOME-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診の剤まとめ出力
      *****************************************************************
       20101-SYOSAIMATOME-WRITE2-SEC      SECTION.
      *
           MOVE    SPACE             TO  RECE46-REC
           INITIALIZE                    RECE46-REC
           MOVE    RECE41-KEY        TO  RECE46-KEY
      *
           MOVE    WRK-SET-LINENUM   TO  RECE46-LINENUM
      *
           COMPUTE IDX9  =  (IDX6 - 1) * 10
           COMPUTE RECE46-ZAINUM       =   99992501 + IDX9
      *
           MOVE    1                 TO  RECE46-ZAIMEISAISU
           MOVE    RECE41-PTNUM      TO  RECE46-PTNUM
           MOVE    SPACE             TO  RECE46-SRYKA
           MOVE    WRK-ZAI           TO  RECE46-TEKIYO-INF
      *
           MOVE    "12"              TO  RECE46-SRYKBN
      *
           MOVE    99999999          TO  RECE46-ZAITEN
           MOVE    ZERO              TO  RECE46-KNSJISKBN
      *
           IF      IDX6  =  1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8
             CONTINUE
           ELSE
             MOVE    9               TO  RECE46-CDKBN-SYO
             MOVE    99              TO  RECE46-CDKBN-BU
             MOVE    "9"             TO  RECE46-CDKBN-KBN
             MOVE    999             TO  RECE46-CDKBN-KBNNUM
             MOVE    99              TO  RECE46-CDKBN-KBNNUM-EDA
             MOVE    99              TO  RECE46-CDKBN-KOUBAN
             MOVE    999999999       TO  RECE46-KOUHYOJUNNUM
           END-IF
      *
           MOVE    SPACE             TO  RECE46-SAFLG
           MOVE    ZERO              TO  RECE46-ZAIKBN
           MOVE    SPACE             TO  RECE46-SRYSYUKBN
           MOVE    ZERO              TO  RECE46-DAY
      *
           MOVE    SPACE             TO  RECE46-JIHOKBN
      *
           WRITE   RECE46-REC
      *
           IF  STS-RECE46        NOT =   ZERO
               DISPLAY "*** ORCR0460 RECE46 WRITE ERR  ***"
               DISPLAY "*** RECE46-SRYKBN ***" RECE46-SRYKBN
               DISPLAY "*** RECE46-ZAINUM ***" RECE46-ZAINUM
               DISPLAY "<SYOSAIMATOME-WRITE2>"
               MOVE    2             TO  SPA-STS
           ELSE
               ADD     1             TO  CNT-RECE46
           END-IF
      *
           .
      *
       20101-SYOSAIMATOME-WRITE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数再計算処理
      *****************************************************************
       400-TENSU-SAIKEISAN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
           MOVE    "＊"                TO  WRK-ZAI-MK
      *    点数・回数セット
           MOVE    WRK-TENSUGOKEI      TO  WRK-ZAITENX
           MOVE    RECE44-ZAIKAISU     TO  WRK-ZAIKAISUX
      *    必要有効桁数・開始位置・回数編集
           PERFORM 810-TENSUKAI-HEN-SEC
      *
           .
       400-TENSU-SAIKEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細空白処理
      *****************************************************************
       410-MEISAI-SPACE-SEC   SECTION.
      *
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY   >       WRK-TBLMAX
               IF  IDY                   =   1
                   MOVE    SPACE         TO  TBL-ZAI (IDY)(3:)
               ELSE
                   INITIALIZE                TBL-ZAI (IDY)
               END-IF
           END-PERFORM
           MOVE    1                 TO  WRK-TBLMAX
      *
           .
       410-MEISAI-SPACE-EXT.
           EXIT.
      *****************************************************************
      *    剤テーブル編集処理
      *****************************************************************
       430-TBLZAI-HEN-SEC     SECTION.
      *
      *    数量編集
           PERFORM 4301-SURYO-HEN1-SEC
      *
      *    分画数編集
           PERFORM 4301-BUNKAKU-HEN1-SEC
      *
      *    明細編集・明細桁数計算
           PERFORM 4302-MEISAI-KETA-HEN1-SEC
      *
      *    剤・診療行為テーブル編集処理
           PERFORM 4303-ZAITBL-TENSO-SEC
      *    
      *    明細が１行に収まりきらない場合
           IF  WRK-WCLM            >   50
               MOVE    SPACE             TO  WRK-ZAI-NAME
               MOVE    WRK-KOUI (51:)    TO  WRK-ZAI-NAME
               PERFORM 4303-ZAITBL-TENSO-SEC
           END-IF
           IF  WRK-WCLM            >   100
               MOVE    SPACE             TO  WRK-ZAI-NAME
               MOVE    WRK-KOUI (101:)   TO  WRK-ZAI-NAME
               PERFORM 4303-ZAITBL-TENSO-SEC
           END-IF
           IF  WRK-WCLM            >   150
               MOVE    SPACE             TO  WRK-ZAI-NAME
               MOVE    WRK-KOUI (151:)   TO  WRK-ZAI-NAME
               PERFORM 4303-ZAITBL-TENSO-SEC
           END-IF
           IF  WRK-WCLM            >   200
               MOVE    SPACE             TO  WRK-ZAI-NAME
               MOVE    WRK-KOUI (201:)   TO  WRK-ZAI-NAME
               PERFORM 4303-ZAITBL-TENSO-SEC
           END-IF
           IF  WRK-WCLM            >   250
               MOVE    SPACE             TO  WRK-ZAI-NAME
               MOVE    WRK-KOUI (251:)   TO  WRK-ZAI-NAME
               PERFORM 4303-ZAITBL-TENSO-SEC
           END-IF
      *
           .
       430-TBLZAI-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       4301-SURYO-HEN1-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      *
           IF      TBL-ZAIW-SRYSURYO (WRK-WIDX1 WRK-WIDX2)
                                   =   ZERO
               GO  TO  4301-SURYO-HEN1-EXT  
           END-IF
      * 
      *    数字表示用編集（半角）
           MOVE    TBL-ZAIW-SRYSURYO (WRK-WIDX1 WRK-WIDX2)
                                   TO  WRK-SURYO
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    「０５９」で始まる場合は小数点以下３桁までとする
           IF      TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) (1:3)
                                   =   "059"
               MOVE    "00"            TO  WRK-SURYO-S2 (4:2)
           END-IF
      *
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY   FROM    5   BY  -1
                     UNTIL     IDY           =   ZERO
             IF  WRK-SURYO-S2(IDY:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-SURYO-S2(IDY:1)
                                         TO  WRK-SURYO-Z3(IDY:1)
             END-IF
           END-PERFORM
           IF  WRK-SURYO-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-SURYO-Z2
           END-IF
      *    表示文字数カウント
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY           >   11
             IF  WRK-SURYO-X (IDY:1)   NOT =   SPACE
                 IF  WRK-SURYOST           =   ZERO
                     MOVE    IDY           TO  WRK-SURYOST
                 END-IF
      *          数量桁数カウント(６桁目：ピリオド)
                 ADD     1                 TO  WRK-SURYOKETA
                 IF  IDY                   <   6
                     ADD     1             TO  WRK-SURYOKETA1
                 END-IF
                 IF  IDY                   >   6
                     ADD     1             TO  WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST             =   6
               MOVE    "0"             TO  WRK-SURYO-Z1(5:1)
               MOVE    5               TO  WRK-SURYOST
               MOVE    1               TO  WRK-SURYOKETA1
               ADD     1               TO  WRK-SURYOKETA
           END-IF
      *    全角変換
           IF  WRK-SURYOKETA1          >   ZERO
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z1 (WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR-CNT
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2          >   ZERO
               MOVE    "．"            TO  WRK-SURYO-N2
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z3 (1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR-CNT
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N3
           END-IF
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           .
       4301-SURYO-HEN1-EXT.
           EXIT.
      *     
      *****************************************************************
      *    分画数編集処理
      *****************************************************************
       4301-BUNKAKU-HEN1-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-BUNKAKUKETA
           MOVE    SPACE           TO  WRK-BUNKAKU-N
      *
           IF      TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) (1:1)
                                   =   "7"
           AND     TBL-ZAIW-TANICD(WRK-WIDX1 WRK-WIDX2)
                                   =   WRK-CONS-TANICD 
           AND     TBL-ZAIW-SRYKAISU(WRK-WIDX1 WRK-WIDX2)
                                   >   1
             IF   TBL-ZAIW-SRYCD(WRK-WIDX1 WRK-WIDX2) =
                  "700020000" OR "700040000" OR "700060000" OR
                  "700070000" OR "700090000" OR "700100000" OR
                  "700120000" OR "700130000" OR "700150000" OR
                  "700300000" OR "700320000" OR "700340000" OR
                  "700350000" OR "700720000" OR "700740000" OR
                  "700760000" OR "700770000" OR "700790000" OR
                  "700800000" OR "700820000" OR "700830000" OR
                  "700850000" OR "700860000" OR "700880000" OR
                  "700890000"
               GO  TO  4301-BUNKAKU-HEN1-EXT
             ELSE
               CONTINUE
             END-IF
           ELSE                            
               GO  TO  4301-BUNKAKU-HEN1-EXT  
           END-IF
      * 
      *    表示用編集（半角）
           MOVE    TBL-ZAIW-SRYKAISU (WRK-WIDX1 WRK-WIDX2)
                                   TO  WRK-SRYKAISU-X
      *    全角変換
           INITIALIZE              WRK-CHAR-HENKAN-AREA
           MOVE    WRK-SRYKAISU-X  TO  WRK-CHAR-X
           MOVE    3               TO  WRK-CHAR-CNT
           MOVE    1               TO  WRK-CHAR-FLG
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           STRING  WRK-CHAR-N      DELIMITED BY  SPACE
                   "分画"          DELIMITED  BY  SIZE
                                   INTO  WRK-BUNKAKU-N
           END-STRING
           COMPUTE WRK-BUNKAKUKETA =   WRK-CHAR-N-CNT  +  4
      *
           .
       4301-BUNKAKU-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細・明細桁　編集処理
      *****************************************************************
       4302-MEISAI-KETA-HEN1-SEC    SECTION.
      *
           MOVE    SPACE           TO  WRK-KOUI
                                       WRK-RECEHENNAME
                                       WRK-RECEHENTANINAME
           MOVE    ZERO            TO  WRK-RECEHENTANI
      *
           IF      TBL-ZAIW-RECEHENFLG (WRK-WIDX1 WRK-WIDX2) 
                                   =   "1"
               STRING    "（"          DELIMITED   BY  SIZE
                         TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-RECEHENNAME
               END-STRING
               MOVE    WRK-RECEHENNAME TO
                                   TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
               COMPUTE TBL-ZAIW-YUKOKETA (WRK-WIDX1 WRK-WIDX2)  =
                       TBL-ZAIW-YUKOKETA (WRK-WIDX1 WRK-WIDX2)  +   1
           END-IF
      *
           IF  TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                   =   SPACE
      *        < 名称 >
               MOVE    TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                   TO  WRK-KOUI
      *        < 名称*2 >
               COMPUTE WRK-WCLM  =
                       TBL-ZAIW-YUKOKETA (WRK-WIDX1 WRK-WIDX2) *  2
           ELSE
      *
               IF   TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)(1:2)
                                            =  "＊"
                   IF      TBL-ZAIW-RECEHENFLG (WRK-WIDX1 WRK-WIDX2) 
                                                =   "1"
                       MOVE    "）"    TO  
                               TBL-ZAIW-TANINAME   (WRK-WIDX1 WRK-WIDX2)
                       MOVE    2       TO  
                               TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                   ELSE
                       MOVE    SPACE   TO  
                              TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                   END-IF
               ELSE
                   IF      TBL-ZAIW-RECEHENFLG (WRK-WIDX1 WRK-WIDX2) 
                                                =   "1"
                       COMPUTE WRK-RECEHENTANI =
                           TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                           *   2
                       STRING    TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                                    (1:WRK-RECEHENTANI)
                                           DELIMITED   BY  SIZE
                             "）"          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEHENTANINAME
                       END-STRING
                       MOVE    WRK-RECEHENTANINAME TO
                               TBL-ZAIW-TANINAME   (WRK-WIDX1 WRK-WIDX2)
                       COMPUTE
                           TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)  =
                           TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                                   +  1
                   END-IF
               END-IF
      *        名称の長さ
               COMPUTE   WRK-NAMEKETA  =
                         TBL-ZAIW-YUKOKETA (WRK-WIDX1 WRK-WIDX2)
                         *   2
      *        -----------------------------------
      *        １明細に数量単位まで収まるかどうか
      *        -----------------------------------
               EVALUATE    TRUE
               WHEN    WRK-NAMEKETA    >   50
      *            明細行
                   MOVE    TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                       TO  WRK-KOUI
                   MOVE    WRK-KOUI    TO  WRK-ZAI-NAME
                   PERFORM 4303-ZAITBL-TENSO-SEC
      *
                   MOVE    SPACE       TO  WRK-KOUI
                   MOVE    SPACE       TO  WRK-ZAI-NAME
                   COMPUTE WRK-NAMEKETA    =   WRK-NAMEKETA  -  50
      *   　       < 名称後半 + 空白 + 数量 + 単位 + 分画数>
                   STRING  TBL-ZAIW-NAME     (WRK-WIDX1 WRK-WIDX2)
                                             (51:WRK-NAMEKETA)
                                              DELIMITED  BY  SIZE
                           "  "               DELIMITED  BY  SIZE
                           WRK-SRYSURYOW      DELIMITED  BY  " "
                           TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                              DELIMITED  BY  " "
                           "  "               DELIMITED  BY  SIZE
                           WRK-BUNKAKU-N      DELIMITED  BY  " "
                           INTO               WRK-KOUI
                   END-STRING
                   COMPUTE   WRK-WCLM
                         =   ( WRK-SURYOKETA                  *   2 )
                         +   ( TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                              *   2 )
                         +   WRK-NAMEKETA  +  2
                   IF      WRK-BUNKAKUKETA        >   ZERO
                       COMPUTE   WRK-WCLM    =   WRK-WCLM         +
                                                 WRK-BUNKAKUKETA  +
                                                 2
                   END-IF                                             
      *
               WHEN    WRK-NAMEKETA    =   50
      *            数量・単位　次行印字処理
                   PERFORM 43021-SURYO-NEXTPRT-SEC
      *
               WHEN    WRK-NAMEKETA    <   50
      *            間の空白分を足し込む
                   COMPUTE   WRK-NAMEKETA  =   WRK-NAMEKETA   +   2
                   IF  WRK-NAMEKETA    <   50
      *                余白
                       COMPUTE   WRK-YOHAKU  =   50  -   WRK-NAMEKETA
      *                数量・分画数桁数
                       COMPUTE   WRK-WKETA   =
                         ( WRK-SURYOKETA                  * 2 )
                             +
                         ( TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                          * 2 )
                       IF      WRK-BUNKAKUKETA   >   ZERO
                           COMPUTE   WRK-WKETA   =   WRK-WKETA        +
                                                     WRK-BUNKAKUKETA  +
                                                     2
                       END-IF                                             
      *
                       IF  WRK-YOHAKU        >=  WRK-WKETA
      *                    < 名称 + 空白 + 数量 + 単位 + 空白 + 分画数>
      ********************STRING  TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
      ********************                        DELIMITED  BY  " "
      ********************         "  "           DELIMITED  BY  SIZE
                          STRING  TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                                (1:WRK-NAMEKETA)
                                                  DELIMITED  BY  SIZE
                                   WRK-SRYSURYOW  DELIMITED  BY  " "
                                   TBL-ZAIW-TANINAME
                                                 (WRK-WIDX1 WRK-WIDX2)
                                                  DELIMITED  BY  " "
                                   "  "           DELIMITED  BY  SIZE
                                   WRK-BUNKAKU-N  DELIMITED  BY  " "
                                   INTO           WRK-KOUI
                           END-STRING
      *                    < 名称*2 + 空白*2 + 数量 + 単位*2 + 空白 + 分画数>
                           COMPUTE   WRK-WCLM     =
                               ( TBL-ZAIW-YUKOKETA 
                                         (WRK-WIDX1 WRK-WIDX2)  * 2 )
                             + ( WRK-SURYOKETA                  * 2 )
                             + ( TBL-ZAIW-TANIMOJISU 
                                         (WRK-WIDX1 WRK-WIDX2)  * 2 )
                             +  2
                           IF      WRK-BUNKAKUKETA   >   ZERO
                               COMPUTE   WRK-WCLM   =   WRK-WCLM      +
                                                     WRK-BUNKAKUKETA  +
                                                     2
                           END-IF                                             
                       ELSE
      *                    数量・単位　次行印字処理
                           PERFORM 43021-SURYO-NEXTPRT-SEC
                       END-IF
                   ELSE
      *                数量・単位　次行印字処理
                       PERFORM 43021-SURYO-NEXTPRT-SEC
                   END-IF
               END-EVALUATE
           END-IF
           MOVE    WRK-KOUI        TO  WRK-ZAI-NAME
      *
           .
       4302-MEISAI-KETA-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量・単位　次行編集処理
      *****************************************************************
       43021-SURYO-NEXTPRT-SEC    SECTION.
      *
      *    < 名称 >
           MOVE    TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                      TO  WRK-KOUI
           MOVE    WRK-KOUI           TO  WRK-ZAI-NAME
           PERFORM 4303-ZAITBL-TENSO-SEC
      *
           MOVE    SPACE              TO  WRK-KOUI
           MOVE    SPACE              TO  WRK-ZAI-NAME
      *    < 空白 + 数量 + 単位 + 空白 + 分画数>
           STRING  "    "             DELIMITED  BY  SIZE
                   WRK-SRYSURYOW      DELIMITED  BY  " "
                   TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                      DELIMITED  BY  " "
                   "  "               DELIMITED  BY  SIZE
                   WRK-BUNKAKU-N      DELIMITED  BY  " "
                   INTO               WRK-KOUI
           END-STRING
           COMPUTE   WRK-WCLM
                 =   ( WRK-SURYOKETA                  *   2 )
                 +   ( TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                      *   2 )
                 +   4
           IF      WRK-BUNKAKUKETA   >   ZERO
               COMPUTE   WRK-WCLM   =   WRK-WCLM         +
                                        WRK-BUNKAKUKETA  +
                                        2
           END-IF                                             
      *
           .
       43021-SURYO-NEXTPRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤・診療行為テーブル編集処理
      *****************************************************************
       4303-ZAITBL-TENSO-SEC    SECTION.
      *
           ADD     1                 TO  WRK-TBLMAX
           IF  WRK-TBLMAX           >   100
               DISPLAY "*** ORCR0460 TBL-ZAI INDEX[100] OVER  ***"
           ELSE
               MOVE    WRK-ZAI       TO  TBL-ZAI (WRK-TBLMAX)
               MOVE    SPACE         TO  WRK-ZAI
           END-IF
      *
           .
       4303-ZAITBL-TENSO-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数回数セット処理
      *****************************************************************
       440-TNSKAI-SET-SEC          SECTION.
      *
      *---- (01.00.05) LINE ADD START --------------------------------
      ***  IF   RECE44-ZAIKBN      =    1 OR 2
           IF   RECE44-ZAIKBN      =    2
             CONTINUE
           ELSE
             IF ((FLG-COMMENT        =    1                  ) AND
                 (RECE44-SRYKBN      =   "99"                ))
             OR ((FLG-COMMENT2       =    1                  ) AND
                 (RECE44-SRYKBN      =   "11" OR "12" OR "99"))
             OR ((FLG-COMMENT3       =    1                  ) AND
                 (RECE44-SRYKBN      =   "11" OR "12"        ) AND
                 (WRK-TENSUGOKEI     =   ZERO                ))
             OR ((FLG-COMMENT3       =    1                  ) AND
                 (RECE44-SRYKBN      NOT =  "11" AND "12"    ) AND
                 (RECE44-ZAITEN      =   ZERO                ) AND
                 (RECE44-TOYAKUFLG   =   SPACE               ) AND
                 (RECE44-YKZKISAIFLG =   SPACE               ))
             OR ( RECE44-SAFLG   NOT =   SPACE                )
             OR ( RECE44-TOYAKUFLG   =   "2" OR "4"           )
             OR ( RECE44-ZAIKBN      =    1                   )
             OR ( RECE44-HOMONCOMFLG NOT = SPACE              )
                  GO  TO  440-TNSKAI-SET-EXT
             END-IF
           END-IF
      *---- (01.00.05) LINE ADD END   --------------------------------
      *         
           MOVE    WRK-TNSKAI-ST     TO  IDA
           COMPUTE   IDA             =   IDA     -   2
           MOVE    WRK-TNSKAI-KETA   TO  IDB
      *****MOVE    WRK-TBLMAX       TO  IDY
      *
      *    最終明細に点数・回数を印字するスペース有り
           MOVE    TBL-ZAI (IDY)(3:)     TO  WRK-ZAI-NAME
           IF  WRK-ZAI-NAME (IDA:)       =   SPACE
               COMPUTE   WRK-TNSKAI-ST   =   22
                                         -   WRK-TNSKAI-KETA
               ADD     1                 TO  WRK-TNSKAI-ST
               MOVE    WRK-TNSKAI (WRK-TNSKAI-ST:IDB)
                                         TO  WRK-ZAI-TNSKAI
                                                    (WRK-TNSKAI-ST:)
               MOVE    WRK-ZAI-NAME      TO  TBL-ZAI (IDY)(3:)
           ELSE
      *    最終明細に点数・回数を印字するスペース無し
               ADD     1                 TO  IDY
               IF  IDY                   >   100
                   DISPLAY "*#* ORCR0460 TBL-ZAI INDEX[100] OVER  ***"
               ELSE
                   MOVE    SPACE         TO  WRK-ZAI
                   MOVE    WRK-TNSKAI    TO  WRK-ZAI-TNSKAI
                   MOVE    WRK-ZAI       TO  TBL-ZAI (IDY)
                   MOVE    SPACE         TO  WRK-ZAI
               END-IF
           END-IF
      *
           .
       440-TNSKAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６出力処理
      *****************************************************************
       450-RECE46-OUT-SEC    SECTION.
      *
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY   >   WRK-TBLMAX
      *
               ADD     1                 TO  WRK-LINENUM
               MOVE    SPACE             TO  RECE46-REC
               INITIALIZE                    RECE46-REC
               MOVE    RECE41-KEY        TO  RECE46-KEY
               MOVE    RECE44-ZAINUM     TO  RECE46-ZAINUM
               MOVE    WRK-LINENUM       TO  RECE46-LINENUM
               MOVE    WRK-TBLMAX        TO  RECE46-ZAIMEISAISU
               MOVE    RECE44-PTNUM      TO  RECE46-PTNUM
               MOVE    RECE44-SRYKA      TO  RECE46-SRYKA
               MOVE    RECE44-SRYKBN     TO  RECE46-SRYKBN
               MOVE    TBL-ZAI (IDY)     TO  RECE46-TEKIYO-INF
      *
               IF    ( FLG-COMMENT2     =   1           )               
               AND   ( RECE44-SRYKBN    =  "11" OR "12" )               
                   MOVE  SPACE           TO  RECE46-SRYKBN              
               END-IF                                     
      *---------適用欄並び替え用の編集（表用番号等）-----------------
               IF      IDY               =   1
                 IF       (WRK-ZAIW-SRYCD1(1:1)  =   "6"  OR  "7")
                 OR       (WRK-ZAIW-SRYCD1(1:3)  =   "059"       )
                 OR       (RECE44-SAFLG     NOT  =   SPACE       )
                 OR       (RECE44-ZAIKBN         =    1          )
                     MOVE    ALL "9"            TO
                                            WRK-ZAIW-CDKBN-AREA
                     IF   (RECE44-SAFLG     NOT  =   SPACE)
                     OR   (RECE44-ZAIKBN         =    1   )
                         MOVE    999999999   TO  WRK-ZAIW-KOUHYOJYUNNUM
                     ELSE
                       IF    WRK-ZAIW-SRYCD1(1:1)  =   "6"
                         MOVE    999999997   TO  WRK-ZAIW-KOUHYOJYUNNUM
                       ELSE
                         MOVE    999999998   TO  WRK-ZAIW-KOUHYOJYUNNUM
                       END-IF
                     END-IF
                 END-IF
                 IF   RECE41-SRYYM   <=   "201306"
                   EVALUATE    WRK-ZAIW-SRYCD1
                     WHEN     "101800175"
                       MOVE      ALL "9"     TO  WRK-ZAIW-CDKBN-AREA
                       MOVE      999999982   TO  WRK-ZAIW-KOUHYOJYUNNUM
                     WHEN     "101800180"
                       MOVE      ALL "9"     TO  WRK-ZAIW-CDKBN-AREA
                       MOVE      999999983   TO  WRK-ZAIW-KOUHYOJYUNNUM
                     WHEN     "101800330"
                       MOVE      ALL "9"     TO  WRK-ZAIW-CDKBN-AREA
                       MOVE      999999984   TO  WRK-ZAIW-KOUHYOJYUNNUM
                     WHEN     "101800340"
                       MOVE      ALL "9"     TO  WRK-ZAIW-CDKBN-AREA
                       MOVE      999999985   TO  WRK-ZAIW-KOUHYOJYUNNUM
                   END-EVALUATE
                 ELSE
                   IF  WRK-ZAIW-SRYCD1(1:3) = "101"
                   AND WRK-ZAIW-CDKBN-KBN   = SPACE
                       MOVE      ALL "9"     TO  WRK-ZAIW-CDKBN-AREA
                       MOVE  WRK-ZAIW-SRYCD1 TO  WRK-ZAIW-KOUHYOJYUNNUM
                   END-IF
                 END-IF
               END-IF
               MOVE    WRK-ZAIW-CDKBN-SYO  TO  RECE46-CDKBN-SYO
               MOVE    WRK-ZAIW-CDKBN-BU   TO  RECE46-CDKBN-BU
               MOVE    WRK-ZAIW-CDKBN-KBN  TO  RECE46-CDKBN-KBN
               MOVE    WRK-ZAIW-CDKBN-KBNNUM
                                           TO  RECE46-CDKBN-KBNNUM
               MOVE    WRK-ZAIW-CDKBN-KBNNUM-EDA
                                           TO  RECE46-CDKBN-KBNNUM-EDA
               MOVE    WRK-ZAIW-CDKBN-KOUBAN
                                           TO  RECE46-CDKBN-KOUBAN
               MOVE    WRK-ZAIW-KOUHYOJYUNNUM
                                           TO  RECE46-KOUHYOJUNNUM
      *
               IF    RECE44-GAZOGEN = "1"
                  MOVE    202              TO  RECE46-CDKBN-KBNNUM
                  MOVE    99               TO  RECE46-CDKBN-KOUBAN
                  MOVE    ZERO             TO  RECE46-KOUHYOJUNNUM
               END-IF
      *
               IF    RECE44-JIHOKBN = SPACE
                  MOVE    RECE44-ZAITEN         TO  RECE46-ZAITEN
               ELSE
                  MOVE    RECE44-HOKATU-ZAITEN  TO  RECE46-ZAITEN
               END-IF
      *
               IF      RECE44-SRYKBN     =   "60" OR "64"
                       MOVE   WRK-ZAIW-KNSJISKBN
                                             TO  RECE46-KNSJISKBN
               ELSE
                       MOVE   ZERO           TO  RECE46-KNSJISKBN
               END-IF
      *
               MOVE       SPACE          TO  RECE46-SRYSYUKBN
               MOVE       ZERO           TO  RECE46-DAY
               MOVE       ZERO           TO  RECE46-DAY2
               EVALUATE  RECE44-SRYKBN
                 WHEN      "21"
                 WHEN      "22"
                 WHEN      "23"
                 WHEN      "33"
                 WHEN      "40"
                   PERFORM   VARYING   IDW3   FROM    1   BY  1
                              UNTIL    IDW3   >      31
                       IF    RECE44-KAISU(1 IDW3)    NOT =   ZERO
                          MOVE   IDW3    TO  RECE46-DAY
                          MOVE    31     TO  IDW3
                       END-IF
                   END-PERFORM
                 WHEN      "50"
                 WHEN      "54"
                   MOVE   WRK-SRYSYUKBN  TO  RECE46-SRYSYUKBN
                   PERFORM   VARYING   IDW3   FROM    1   BY  1
                              UNTIL    IDW3   >      31
                       IF    RECE44-KAISU(1 IDW3)    NOT =   ZERO
                          MOVE   IDW3    TO  RECE46-DAY
                          MOVE    31     TO  IDW3
                       END-IF
                   END-PERFORM
                 WHEN      "70"
                   IF    RECE44-GAZOGEN = "1"
                     PERFORM   VARYING   IDW3   FROM    1   BY  1
                                UNTIL    IDW3   >      31
                       IF    RECE44-KAISU(1 IDW3)    NOT =   ZERO
                          MOVE   IDW3    TO  RECE46-DAY2
                          MOVE    31     TO  IDW3
                       END-IF
                     END-PERFORM
                   END-IF
                 WHEN      "99"
                   MOVE   WRK-SRYSYUKBN  TO  RECE46-SRYSYUKBN
               END-EVALUATE
      *
               MOVE    RECE44-SAFLG      TO  RECE46-SAFLG
               MOVE    RECE44-ZAIKBN     TO  RECE46-ZAIKBN
      *
               MOVE    RECE44-OKIKAEFLG  TO  RECE46-OKIKAEFLG
      *
               IF      RECE44-SRYKBN     =   "80"
                 IF (WRK-ZAIW-SRYCD1 = "120002710" OR "120002810" OR
                                       "120002910" OR "120003010" OR
                                       "120002470" OR "120002570" OR
                                       "120003270" OR "120003470" OR
                                       "120003570" OR "120003710" OR
                                       "120003970" OR "120004270" OR
                                       "120004570" OR "120004610")
                 OR (RECE44-SIPPUCOMFLG  =   "1"                 )
                 OR (RECE44-SIPPUFLG     =   "1"                 )
                       MOVE   ZERO   TO   RECE46-SYOHOUFLG
                 ELSE
                       MOVE    1     TO   RECE46-SYOHOUFLG
                 END-IF
                 IF (RECE44-SIPPUCOMFLG  =   "1")
                 OR (RECE44-SIPPUFLG     =   "1")
                       MOVE    9     TO   RECE46-CDKBN-SYO
                       MOVE    99    TO   RECE46-CDKBN-BU
                       MOVE    "9"   TO   RECE46-CDKBN-KBN
                       MOVE    999   TO   RECE46-CDKBN-KBNNUM
                       MOVE    99    TO   RECE46-CDKBN-KBNNUM-EDA
                       MOVE    99    TO   RECE46-CDKBN-KOUBAN
                       MOVE    999999999
                                     TO   RECE46-KOUHYOJUNNUM
                 END-IF
               END-IF
      *
               MOVE    RECE44-JIHOKBN    TO  RECE46-JIHOKBN
      *
               WRITE   RECE46-REC
               IF  STS-RECE46          NOT =   ZERO
                   DISPLAY "*** ORCR0460 RECE46 WRITE ERR  ***"
                   DISPLAY "*** RECE46-SRYKBN ***" RECE46-SRYKBN
                   DISPLAY "*** RECE46-ZAINUM ***" RECE46-ZAINUM
                   MOVE   WRK-TBLMAX     TO  IDY
                   MOVE    2             TO  SPA-STS
               ELSE
                   ADD     1             TO  CNT-RECE46
               END-IF
           END-PERFORM
      *     
           INITIALIZE                    TBL-ZAI-AREA
      *
           .
       450-RECE46-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理開始処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           MOVE    2                   TO  SPA-STS
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE41-FILE
           CLOSE                       RECE44-FILE
           CLOSE                       RECE45-FILE
           CLOSE                       RECE46-FILE     
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41  IN  " CNT-RECE41   
           DISPLAY "RECE46  CNT " CNT-RECE46 
           DISPLAY "*** ORCR0460 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値半角→全角変換処理
      *****************************************************************
       800-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
      *    FLG=1のとき空白は編集しない
      *
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *     
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   WRK-CHAR-CNT
               IF      WRK-CHAR-FLG        =   1
               AND     WRK-CHAR-X (IDZ:1)  =   SPACE
                   CONTINUE
               ELSE        
                   IF    WRK-CHAR-X (IDZ:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDZ:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDZ:1)  TO  WRK-NUM
                           MOVE    TBL-SUJI (WRK-NUM)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF                    
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF    
           END-PERFORM
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数・回数編集処理
      *****************************************************************
       810-TENSUKAI-HEN-SEC    SECTION.
      *    点数編集
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-ZAITENX         TO  WRK-CHAR-X
           MOVE    7                   TO  WRK-CHAR-CNT
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  WRK-ZAITEN
      *    回数編集
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-ZAIKAISUX       TO  WRK-CHAR-X
           MOVE    3                   TO  WRK-CHAR-CNT
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  WRK-ZAIKAISU
           MOVE    "×"                TO  WRK-X
      *
      *    薬剤料逓減のとき点数のまえに▲を表示
           IF      FLG-TEIGEN          =   1
               PERFORM VARYING IDZ FROM    13  BY  -2
                       UNTIL   IDZ <       3
                   IF      WRK-ZAITEN (IDZ:2)  =   SPACE
                       MOVE    "▲"        TO  WRK-ZAITEN (IDZ:2)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
      *
      *
      *    必要有効桁数（点数ZZZZZZZ X 数量ZZZ の全角の桁数）
           MOVE    ZERO                TO  WRK-TNSKAI-KETA
           PERFORM VARYING   IDZ     FROM    1   BY  1
                   UNTIL     IDZ     >       22
               IF      WRK-TNSKAI (IDZ:1)      NOT =   SPACE
                   COMPUTE WRK-TNSKAI-KETA   =   22  -  IDZ     +   1
                   MOVE    22                  TO  IDZ
               END-IF 
           END-PERFORM
      *    開始位置を取得
           COMPUTE   WRK-TNSKAI-ST     =   ( 50  -  WRK-TNSKAI-KETA )
                                       +   1
      *
           .
       810-TENSUKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    2           TO  SPA-STS
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                   ADD     1           TO  CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                   IF    (WRK-PARA-RECEKBN       =  "4")
                   AND   (WRK-PARA-JIBAIPRTKBN   =  "2")
                       GO  TO  990-RECE41-READ-SEC
                   END-IF
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE44
                   ADD     1         TO  CNT-IN-CANCEL
                   IF  ( RECE44-MEISAISU       =   ZERO  )
                   OR  ((WRK-PARA-PRTKBN       =   "1"  ) AND
                        (RECE44-TOYAKUFLG  NOT =   SPACE))
                       GO  TO  990-RECE44-NEXT-SEC
                   END-IF       
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
