      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ********************************************************************
      *    後期高齢者請求書 （島根県）     COPY (SKT3215.INC) 
      *
      *  作成日付    作業者        記述
      *  08/05/02    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT3215.
      *    診療年月
           03  SKT3215-SRYYM                    PIC X(16).
      *    請求年月日
           03  SKT3215-SEIYMD                   PIC X(22).
      *    医療機関住所
           03  SKT3215-ADRS                     PIC X(100).
      *    医療機関名称
           03  SKT3215-HOSPNAME                 PIC X(100).
      *    開設者氏名
           03  SKT3215-KAISETUNAME              PIC X(40).
      *    電話番号
           03  SKT3215-TEL                      PIC X(15).
      *    保険者名
           03  SKT3215-HKNJANAME                PIC X(40).
      *    医療機関コード
           03  SKT3215-HOSPCD                   PIC X(14).
      *    在医総・在総診
           03  SKT3215-ZAI1                     PIC X(02).
           03  SKT3215-ZAI2                     PIC X(02).
      *    ページ番号
           03  SKT3215-NO                       PIC X(04).
      *    請求テーブル
           03  SKT3215-SEIKYU-TBL      OCCURS  2.
      *    入外テーブル
               05  SKT3215-NYUGAI-TBL      OCCURS  2.
      *    件数
                   07  SKT3215-KENSU            PIC X(05).
      *    診療実日数
                   07  SKT3215-NISSU            PIC X(05).
      *    点数
                   07  SKT3215-TENSU            PIC X(09).
      *    一部負担金
                   07  SKT3215-ITBFTN           PIC X(09).
      *    長期
                   07  SKT3215-TYOKI            PIC X(05).
      *    食事件数
               05  SKT3215-S-KENSU              PIC X(05).
      *    食事日数
               05  SKT3215-S-NISSU              PIC X(05).
      *    金額
               05  SKT3215-S-KINGK              PIC X(09).
      *    標準負担額
               05  SKT3215-S-FTNGK              PIC X(09).
      *    既定公費テーブル
           03  SKT3215-KITEI-TBL       OCCURS  13.
      *    件数
               05  SKT3215-KIKENSU              PIC X(05).
      *    点数
               05  SKT3215-KITENSU              PIC X(09).
      *    患者負担額
               05  SKT3215-KIPTFTN              PIC X(09).
      *    食事件数
               05  SKT3215-S-KIKENSU            PIC X(05).
      *    金額
               05  SKT3215-S-KIKINGK            PIC X(09).
      *    負担額
               05  SKT3215-S-KIFTNGK            PIC X(09).
      *    公費テーブル
           03  SKT3215-KOUHI-TBL       OCCURS  2.
      *    法別番号
               05  SKT3215-HBTNUM               PIC X(02).
      *    公費名
               05  SKT3215-KOHNAME              PIC X(10).
      *    件数
               05  SKT3215-KKENSU               PIC X(05).
      *    点数
               05  SKT3215-KTENSU               PIC X(09).
      *    患者負担額
               05  SKT3215-KPTFTN               PIC X(09).
      *    食事件数
               05  SKT3215-S-KKENSU             PIC X(05).
      *    金額
               05  SKT3215-S-KKINGK             PIC X(09).
      *    負担額
               05  SKT3215-S-KFTNGK             PIC X(09).
