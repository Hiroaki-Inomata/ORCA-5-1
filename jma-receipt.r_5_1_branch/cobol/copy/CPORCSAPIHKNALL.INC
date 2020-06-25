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
      ************************************************************
      *    ＡＰＩ全保険組合せ編集パラメタ　　COPY (CPORCSAPIHKNALL.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  20/04/XX  被保険者証枝番追加
      ************************************************************
       01  ORCSAPIHKNALLAREA.
      *入力
           03  ORCSAPIHKN-PTID             PIC 9(10).
      *処理区分（１：処理日付のみ対象（負担割合を処理日付で決定）
           03  ORCSAPIHKN-KBN              PIC X(01).
      *処理日付
           03  ORCSAPIHKN-SYSYMD           PIC X(08).
      *処理開始年月
           03  ORCSAPIHKN-START-YM         PIC X(06).
      *処理終了年月
           03  ORCSAPIHKN-END-YM           PIC X(06).
      *生年月日（処理区分＝１のみ）
           03  ORCSAPIHKN-BIRTHDAY         PIC X(08).
      *
      *出力領域
      *有効保険一覧
           03  ORCSAPIHKN-HKNCOMBI-AREA.
               05  ORCSAPIHKN-HKNCOMBI-TBL     OCCURS   200.
      *            保険組合せ
                   07  ORCSAPIHKN-THKNCOMBI        PIC  X(04).
      *            保険組合せ負担割合（外来）
                   07  ORCSAPIHKN-COMB-RATEG       PIC  X(04).
      *            保険組合せ負担割合（入院）
                   07  ORCSAPIHKN-COMB-RATEN       PIC  X(04).
      *            保険組合せ有効期間
                   07  ORCSAPIHKN-COMB-TEKSTYMD    PIC  X(10).
                   07  ORCSAPIHKN-COMB-TEKEDYMD    PIC  X(10).
      *
                   07  ORCSAPIHKN-COMBN-TEKSTYMD   PIC  X(08).
                   07  ORCSAPIHKN-COMBN-TEKEDYMD   PIC  X(08).
      *
      *            保険種別
                   07  ORCSAPIHKN-HKNNUM           PIC  X(03).
      *            保険種別名称
                   07  ORCSAPIHKN-HKNNUM-NAME      PIC  X(20).
      *            保険者番号
                   07  ORCSAPIHKN-HKNJANUM         PIC  X(08).
      *            記号
                   07  ORCSAPIHKN-KIGO             PIC  X(80).
      *            番号
                   07  ORCSAPIHKN-NUM              PIC  X(80).
      *R02.4       番号枝番
                   07  ORCSAPIHKN-EDABAN           PIC  X(02).
      *            継続区分
                   07  ORCSAPIHKN-CONTKBN          PIC  X(01).
      *            補助区分
                   07  ORCSAPIHKN-HOJOKBN          PIC  X(01).
      *            補助区分名称
                   07  ORCSAPIHKN-HOJOKBN-NAME     PIC  X(30).
      *            本人家族区分
                   07  ORCSAPIHKN-HONKZKKBN        PIC  X(01).
      *            被保険者名称
                   07  ORCSAPIHKN-HIHKNJANAME      PIC  X(100).
      *            適用期間（表示）
                   07  ORCSAPIHKN-TEKSTYMD         PIC  X(10).
                   07  ORCSAPIHKN-TEKEDYMD         PIC  X(10).
      *            適用期間
                   07  ORCSAPIHKN-NAI-TEKSTYMD     PIC  X(08).
                   07  ORCSAPIHKN-NAI-TEKEDYMD     PIC  X(08).
      *            資格取得日
                   07  ORCSAPIHKN-SKKGETYMD        PIC  X(10).
                   07  ORCSAPIHKN-NAI-SKKGETYMD    PIC  X(08).
      *            確認日付
                   07  ORCSAPIHKN-KAKUNINYMD       PIC  X(10).
                   07  ORCSAPIHKN-NAI-KAKUNINYMD   PIC  X(08).
      *            保険ＩＤ
                   07  ORCSAPIHKN-NAI-HKNID        PIC  9(10).
      *            労災区分
                   07  ORCSAPIHKN-RSI-HKNKBN       PIC  X(01).
      *            非表示区分
                   07  ORCSAPIHKN-HYOJIKBN         PIC  X(01).
      *            公費種類・公費ＩＤ
                   07  ORCSAPIHKN-KOH-G.
                       09  ORCSAPIHKN-KOH-OCC      OCCURS   4.
                           11  ORCSAPIHKN-KOHNUM           PIC  X(03).
                           11  ORCSAPIHKN-KOHNUM-NAME      PIC  X(20).
                           11  ORCSAPIHKN-FTNJANUM         PIC  X(08).
                           11  ORCSAPIHKN-JKYSNUM          PIC  X(20).
      *                    適用期間
                           11  ORCSAPIHKN-KOH-TEKSTYMD     PIC  X(10).
                           11  ORCSAPIHKN-KOH-TEKEDYMD     PIC  X(10).
      *R01.6
      *                    確認日付
                           11  ORCSAPIHKN-KOH-KAKUNINYMD   PIC  X(10).
      *                    入院−負担率
                           11  ORCSAPIHKN-RATE-ADMISSION   PIC X(05).
      *                    入院−固定額
                           11  ORCSAPIHKN-MONEY-ADMISSION  PIC X(06).
      *                    外来−負担率
                           11  ORCSAPIHKN-RATE-OUTPATIENT  PIC X(05).
      *                     外来−固定額
                           11  ORCSAPIHKN-MONEY-OUTPATIENT PIC X(06).
      *                    公費ＩＤ
                           11  ORCSAPIHKN-NAI-KOHID        PIC 9(10).
      *
      *            労災保険情報
                   07  ORCSAPIHKN-RSIINF-AREA.
      *                労災保険名称（記号と同じ内容）
                       09  ORCSAPIHKN-ACCIDENT-NAME        PIC X(50).
      *                傷病の部位（番号と同じ内容）
                       09  ORCSAPIHKN-DISEASE-LOCATION     PIC X(100).
      *                傷病年月日
                       09  ORCSAPIHKN-DISEASE-DATE         PIC X(10).
      *労災
      *                労働保険番号
                       09  ORCSAPIHKN-ACCIDENT-NUMBER      PIC X(14).
      *                年金証書番号
                       09  ORCSAPIHKN-PENSIONCERTIFICATE   PIC X(09).
      *                災害区分
                       09  ORCSAPIHKN-ACCIDENT-CLASS       PIC X(01).
      *                災害区分（名称）
                       09  ORCSAPIHKN-ACCIDENT-CLASS-NAME  PIC X(20).
      *                労働基準監督署コード
                       09  ORCSAPIHKN-LABOR-ST-CODE        PIC X(05).
      *                労働基準監督署コード名称
                       09  ORCSAPIHKN-LABOR-ST-NAME        PIC X(50).
      *
      *                新継再別区分
                       09  ORCSAPIHKN-SINKEI               PIC X(01).
                       09  ORCSAPIHKN-SINKEI-NAME          PIC X(10).
      *                転帰事由
                       09  ORCSAPIHKN-TENKI                PIC X(01).
                       09  ORCSAPIHKN-TENKI-NAME           PIC X(10).
      *                四肢特例区分
                       09  ORCSAPIHKN-SISIKBN              PIC X(01).
                       09  ORCSAPIHKN-SISIKBN-NAME         PIC X(10).
      *
      *                事業所名称
                       09  ORCSAPIHKN-RSI-JIGYOUNAME       PIC X(80).
      *                所在地都道府県名
                       09  ORCSAPIHKN-RSI-PREFNAME         PIC X(20).
      *                所在地都道府県区分
                       09  ORCSAPIHKN-RSI-PREFKBN          PIC X(01).
      *                所在地都道府県区分名称
                       09  ORCSAPIHKN-RSI-PREFKBNNAME      PIC X(02).
      *                所在地郡市区名
                       09  ORCSAPIHKN-RSI-CITYNAME         PIC X(80).
      *                所在地郡市区区分
                       09  ORCSAPIHKN-RSI-CITYKBN          PIC X(01).
      *                所在地郡市区名称
                       09  ORCSAPIHKN-RSI-CITYKBNNAME      PIC X(02).
      *                労災レセ回数記載 基準年月
                       09  ORCSAPIHKN-KIJYUNYM             PIC X(07).
      *                労災レセ回数記載 回数
                       09  ORCSAPIHKN-RESEPUTO-COUNT       PIC X(03).
      *自賠責
      *                自賠責請求区分
                       09  ORCSAPIHKN-JIBAISEIKBN          PIC X(01).
                       09  ORCSAPIHKN-JIBAISEIKBN-NAME     PIC X(20).
      *                自賠責保険会社名
                       09  ORCSAPIHKN-RSI-HOKENNAME        PIC X(80).
      *                自賠責担当医
                       09  ORCSAPIHKN-RSI-DRCD             PIC X(05).
      *                自賠責担当医名称
                       09  ORCSAPIHKN-RSI-DRCD-NAME        PIC X(100).
      *アフターケア
      *                健康管理手帳番号
                       09  ORCSAPIHKN-RSI-KANRITCHO-NUMBER PIC X(13).
      *                損傷区分
                       09  ORCSAPIHKN-DAMAGE-CLASS         PIC X(03).
      *                損傷区分名称
                       09  ORCSAPIHKN-DAMAGE-CLASS-NAME    PIC X(100).
      *第三者行為
      *                現物支給区分
                       09  ORCSAPIHKN-DS-GENBTKBN          PIC X(01).
                       09  ORCSAPIHKN-DS-GENBTKBN-NAME     PIC X(20).
      *                特記事項区分
                       09  ORCSAPIHKN-DS-TOKKIKBN          PIC X(01).
                       09  ORCSAPIHKN-DS-TOKKIKBN-NAME     PIC X(20).
      *
      *        対象保険組合せ件数
               05  ORCSAPIHKN-OT-MAX           PIC 9(04).
      *エラーコード
           03  ORCSAPIHKN-ERRCD                    PIC 9(01).
      *
      *
