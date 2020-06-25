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
        02  Y01.
      ***   02  Y01-FIXED1.
              03  Y01-CALENDAR1.
                04  Y01-YEAR    PIC S9(9)   BINARY.
                04  Y01-MONTH   PIC S9(9)   BINARY.
                04  Y01-DAY PIC S9(9)   BINARY.
              03  Y01-SELNUM.
                04  Y01-VALUE   PIC X(3).
              03  Y01-PTNUM-G.
                04  Y01-PTNUM-STATE   PIC S9(9)   BINARY.
                04  Y01-PTNUM   PIC X(20).
              03  Y01-YYKNAME-G.
                04  Y01-YYKNAME-STATE   PIC S9(9)   BINARY.
                04  Y01-YYKNAME   PIC X(50).
              03  Y01-VIEWPORT1.
                04  Y01-FIXED2.
                  05  Y01-CH-KENSUU01.
                    06  Y01-CH-KENSUU01-STYLE   PIC X(20).
                    06  Y01-KENSUU01            PIC X(5).
                  05  Y01-CH-KENSUU02.
                    06  Y01-CH-KENSUU02-STYLE   PIC X(20).
                    06  Y01-KENSUU02            PIC X(5).
                  05  Y01-CH-KENSUU03.
                    06  Y01-CH-KENSUU03-STYLE   PIC X(20).
                    06  Y01-KENSUU03            PIC X(5).
                  05  Y01-CH-KENSUU04.
                    06  Y01-CH-KENSUU04-STYLE   PIC X(20).
                    06  Y01-KENSUU04            PIC X(5).
                  05  Y01-CH-KENSUU05.
                    06  Y01-CH-KENSUU05-STYLE   PIC X(20).
                    06  Y01-KENSUU05            PIC X(5).
                  05  Y01-CH-KENSUU06.
                    06  Y01-CH-KENSUU06-STYLE   PIC X(20).
                    06  Y01-KENSUU06            PIC X(5).
                  05  Y01-CH-KENSUU07.
                    06  Y01-CH-KENSUU07-STYLE   PIC X(20).
                    06  Y01-KENSUU07            PIC X(5).
                  05  Y01-CH-KENSUU08.
                    06  Y01-CH-KENSUU08-STYLE   PIC X(20).
                    06  Y01-KENSUU08            PIC X(5).
                  05  Y01-CH-KENSUU09.
                    06  Y01-CH-KENSUU09-STYLE   PIC X(20).
                    06  Y01-KENSUU09            PIC X(5).
                  05  Y01-CH-KENSUU10.
                    06  Y01-CH-KENSUU10-STYLE   PIC X(20).
                    06  Y01-KENSUU10            PIC X(5).
                  05  Y01-CH-KENSUU11.
                    06  Y01-CH-KENSUU11-STYLE   PIC X(20).
                    06  Y01-KENSUU11            PIC X(5).
                  05  Y01-CH-KENSUU12.
                    06  Y01-CH-KENSUU12-STYLE   PIC X(20).
                    06  Y01-KENSUU12            PIC X(5).
                  05  Y01-CH-KENSUU13.
                    06  Y01-CH-KENSUU13-STYLE   PIC X(20).
                    06  Y01-KENSUU13            PIC X(5).
                  05  Y01-CH-KENSUU14.
                    06  Y01-CH-KENSUU14-STYLE   PIC X(20).
                    06  Y01-KENSUU14            PIC X(5).
                  05  Y01-CH-KENSUU15.
                    06  Y01-CH-KENSUU15-STYLE   PIC X(20).
                    06  Y01-KENSUU15            PIC X(5).
                  05  Y01-CH-KENSUU16.
                    06  Y01-CH-KENSUU16-STYLE   PIC X(20).
                    06  Y01-KENSUU16            PIC X(5).
                  05  Y01-CH-KENSUU17.
                    06  Y01-CH-KENSUU17-STYLE   PIC X(20).
                    06  Y01-KENSUU17            PIC X(5).
                  05  Y01-CH-KENSUU18.
                    06  Y01-CH-KENSUU18-STYLE   PIC X(20).
                    06  Y01-KENSUU18            PIC X(5).
                  05  Y01-CH-KENSUU19.
                    06  Y01-CH-KENSUU19-STYLE   PIC X(20).
                    06  Y01-KENSUU19            PIC X(5).
                  05  Y01-CH-KENSUU20.
                    06  Y01-CH-KENSUU20-STYLE   PIC X(20).
                    06  Y01-KENSUU20            PIC X(5).
                  05  Y01-CH-KENSUU21.
                    06  Y01-CH-KENSUU21-STYLE   PIC X(20).
                    06  Y01-KENSUU21            PIC X(5).
                  05  Y01-CH-KENSUU22.
                    06  Y01-CH-KENSUU22-STYLE   PIC X(20).
                    06  Y01-KENSUU22            PIC X(5).
                  05  Y01-CH-KENSUU23.
                    06  Y01-CH-KENSUU23-STYLE   PIC X(20).
                    06  Y01-KENSUU23            PIC X(5).
                  05  Y01-CH-KENSUU24.
                    06  Y01-CH-KENSUU24-STYLE   PIC X(20).
                    06  Y01-KENSUU24            PIC X(5).
                  05  Y01-CH-KENSUU25.
                    06  Y01-CH-KENSUU25-STYLE   PIC X(20).
                    06  Y01-KENSUU25            PIC X(5).
                  05  Y01-CH-KENSUU26.
                    06  Y01-CH-KENSUU26-STYLE   PIC X(20).
                    06  Y01-KENSUU26            PIC X(5).
                  05  Y01-CH-KENSUU27.
                    06  Y01-CH-KENSUU27-STYLE   PIC X(20).
                    06  Y01-KENSUU27            PIC X(5).
                  05  Y01-CH-KENSUU28.
                    06  Y01-CH-KENSUU28-STYLE   PIC X(20).
                    06  Y01-KENSUU28            PIC X(5).
                  05  Y01-CH-KENSUU29.
                    06  Y01-CH-KENSUU29-STYLE   PIC X(20).
                    06  Y01-KENSUU29            PIC X(5).
                  05  Y01-CH-KENSUU30.
                    06  Y01-CH-KENSUU30-STYLE   PIC X(20).
                    06  Y01-KENSUU30            PIC X(5).
                  05  Y01-CH-KENSUU31.
                    06  Y01-CH-KENSUU31-STYLE   PIC X(20).
                    06  Y01-KENSUU31            PIC X(5).
                  05  Y01-CH-KENSUU32.
                    06  Y01-CH-KENSUU32-STYLE   PIC X(20).
                    06  Y01-KENSUU32            PIC X(5).
                  05  Y01-CH-KENSUU33.
                    06  Y01-CH-KENSUU33-STYLE   PIC X(20).
                    06  Y01-KENSUU33            PIC X(5).
                  05  Y01-CH-KENSUU34.
                    06  Y01-CH-KENSUU34-STYLE   PIC X(20).
                    06  Y01-KENSUU34            PIC X(5).
                  05  Y01-CH-KENSUU35.
                    06  Y01-CH-KENSUU35-STYLE   PIC X(20).
                    06  Y01-KENSUU35            PIC X(5).
                  05  Y01-CH-KENSUU36.
                    06  Y01-CH-KENSUU36-STYLE   PIC X(20).
                    06  Y01-KENSUU36            PIC X(5).
                  05  Y01-CH-KENSUU37.
                    06  Y01-CH-KENSUU37-STYLE   PIC X(20).
                    06  Y01-KENSUU37            PIC X(5).
                  05  Y01-CH-KENSUU38.
                    06  Y01-CH-KENSUU38-STYLE   PIC X(20).
                    06  Y01-KENSUU38            PIC X(5).
                  05  Y01-CH-KENSUU39.
                    06  Y01-CH-KENSUU39-STYLE   PIC X(20).
                    06  Y01-KENSUU39            PIC X(5).
                  05  Y01-CH-KENSUU40.
                    06  Y01-CH-KENSUU40-STYLE   PIC X(20).
                    06  Y01-KENSUU40            PIC X(5).
                  05  Y01-CH-KENSUU41.
                    06  Y01-CH-KENSUU41-STYLE   PIC X(20).
                    06  Y01-KENSUU41            PIC X(5).
                  05  Y01-CH-KENSUU42.
                    06  Y01-CH-KENSUU42-STYLE   PIC X(20).
                    06  Y01-KENSUU42            PIC X(5).
                  05  Y01-CH-KENSUU43.
                    06  Y01-CH-KENSUU43-STYLE   PIC X(20).
                    06  Y01-KENSUU43            PIC X(5).
                  05  Y01-CH-KENSUU44.
                    06  Y01-CH-KENSUU44-STYLE   PIC X(20).
                    06  Y01-KENSUU44            PIC X(5).
                  05  Y01-CH-KENSUU45.
                    06  Y01-CH-KENSUU45-STYLE   PIC X(20).
                    06  Y01-KENSUU45            PIC X(5).
                  05  Y01-CH-KENSUU46.
                    06  Y01-CH-KENSUU46-STYLE   PIC X(20).
                    06  Y01-KENSUU46            PIC X(5).
                  05  Y01-CH-KENSUU47.
                    06  Y01-CH-KENSUU47-STYLE   PIC X(20).
                    06  Y01-KENSUU47            PIC X(5).
                  05  Y01-CH-KENSUU48.
                    06  Y01-CH-KENSUU48-STYLE   PIC X(20).
                    06  Y01-KENSUU48            PIC X(5).
                  05  Y01-CH-KENSUU49.
                    06  Y01-CH-KENSUU49-STYLE   PIC X(20).
                    06  Y01-KENSUU49            PIC X(5).
                  05  Y01-CH-KENSUU50.
                    06  Y01-CH-KENSUU50-STYLE   PIC X(20).
                    06  Y01-KENSUU50            PIC X(5).
                  05  Y01-CH-KENSUU51.
                    06  Y01-CH-KENSUU51-STYLE   PIC X(20).
                    06  Y01-KENSUU51            PIC X(5).
                  05  Y01-CH-KENSUU52.
                    06  Y01-CH-KENSUU52-STYLE   PIC X(20).
                    06  Y01-KENSUU52            PIC X(5).
                  05  Y01-CH-KENSUU53.
                    06  Y01-CH-KENSUU53-STYLE   PIC X(20).
                    06  Y01-KENSUU53            PIC X(5).
                  05  Y01-CH-KENSUU54.
                    06  Y01-CH-KENSUU54-STYLE   PIC X(20).
                    06  Y01-KENSUU54            PIC X(5).
                  05  Y01-CH-KENSUU55.
                    06  Y01-CH-KENSUU55-STYLE   PIC X(20).
                    06  Y01-KENSUU55            PIC X(5).
                  05  Y01-CH-KENSUU56.
                    06  Y01-CH-KENSUU56-STYLE   PIC X(20).
                    06  Y01-KENSUU56            PIC X(5).
                  05  Y01-CH-KENSUU57.
                    06  Y01-CH-KENSUU57-STYLE   PIC X(20).
                    06  Y01-KENSUU57            PIC X(5).
                  05  Y01-CH-KENSUU58.
                    06  Y01-CH-KENSUU58-STYLE   PIC X(20).
                    06  Y01-KENSUU58            PIC X(5).
                  05  Y01-CH-KENSUU59.
                    06  Y01-CH-KENSUU59-STYLE   PIC X(20).
                    06  Y01-KENSUU59            PIC X(5).
                  05  Y01-CH-KENSUU60.
                    06  Y01-CH-KENSUU60-STYLE   PIC X(20).
                    06  Y01-KENSUU60            PIC X(5).
                  05  Y01-CH-KENSUU61.
                    06  Y01-CH-KENSUU61-STYLE   PIC X(20).
                    06  Y01-KENSUU61            PIC X(5).
                  05  Y01-CH-KENSUU62.
                    06  Y01-CH-KENSUU62-STYLE   PIC X(20).
                    06  Y01-KENSUU62            PIC X(5).
                  05  Y01-CH-KENSUU63.
                    06  Y01-CH-KENSUU63-STYLE   PIC X(20).
                    06  Y01-KENSUU63            PIC X(5).
                  05  Y01-CH-KENSUU64.
                    06  Y01-CH-KENSUU64-STYLE   PIC X(20).
                    06  Y01-KENSUU64            PIC X(5).
                  05  Y01-CH-KENSUU65.
                    06  Y01-CH-KENSUU65-STYLE   PIC X(20).
                    06  Y01-KENSUU65            PIC X(5).
                  05  Y01-CH-KENSUU66.
                    06  Y01-CH-KENSUU66-STYLE   PIC X(20).
                    06  Y01-KENSUU66            PIC X(5).
                  05  Y01-CH-KENSUU67.
                    06  Y01-CH-KENSUU67-STYLE   PIC X(20).
                    06  Y01-KENSUU67            PIC X(5).
                  05  Y01-CH-KENSUU68.
                    06  Y01-CH-KENSUU68-STYLE   PIC X(20).
                    06  Y01-KENSUU68            PIC X(5).
                  05  Y01-CH-KENSUU69.
                    06  Y01-CH-KENSUU69-STYLE   PIC X(20).
                    06  Y01-KENSUU69            PIC X(5).
                  05  Y01-CH-KENSUU70.
                    06  Y01-CH-KENSUU70-STYLE   PIC X(20).
                    06  Y01-KENSUU70            PIC X(5).
                  05  Y01-CH-KENSUU71.
                    06  Y01-CH-KENSUU71-STYLE   PIC X(20).
                    06  Y01-KENSUU71            PIC X(5).
                  05  Y01-CH-KENSUU72.
                    06  Y01-CH-KENSUU72-STYLE   PIC X(20).
                    06  Y01-KENSUU72            PIC X(5).
                  05  Y01-CH-KENSUU73.
                    06  Y01-CH-KENSUU73-STYLE   PIC X(20).
                    06  Y01-KENSUU73            PIC X(5).
                  05  Y01-CH-KENSUU74.
                    06  Y01-CH-KENSUU74-STYLE   PIC X(20).
                    06  Y01-KENSUU74            PIC X(5).
                  05  Y01-CH-KENSUU75.
                    06  Y01-CH-KENSUU75-STYLE   PIC X(20).
                    06  Y01-KENSUU75            PIC X(5).
                  05  Y01-CH-KENSUU76.
                    06  Y01-CH-KENSUU76-STYLE   PIC X(20).
                    06  Y01-KENSUU76            PIC X(5).
                  05  Y01-CH-KENSUU77.
                    06  Y01-CH-KENSUU77-STYLE   PIC X(20).
                    06  Y01-KENSUU77            PIC X(5).
                  05  Y01-CH-KENSUU78.
                    06  Y01-CH-KENSUU78-STYLE   PIC X(20).
                    06  Y01-KENSUU78            PIC X(5).
                  05  Y01-CH-KENSUU79.
                    06  Y01-CH-KENSUU79-STYLE   PIC X(20).
                    06  Y01-KENSUU79            PIC X(5).
                  05  Y01-CH-KENSUU80.
                    06  Y01-CH-KENSUU80-STYLE   PIC X(20).
                    06  Y01-KENSUU80            PIC X(5).
                  05  Y01-CH-KENSUU81.
                    06  Y01-CH-KENSUU81-STYLE   PIC X(20).
                    06  Y01-KENSUU81            PIC X(5).
                  05  Y01-CH-KENSUU82.
                    06  Y01-CH-KENSUU82-STYLE   PIC X(20).
                    06  Y01-KENSUU82            PIC X(5).
                  05  Y01-CH-KENSUU83.
                    06  Y01-CH-KENSUU83-STYLE   PIC X(20).
                    06  Y01-KENSUU83            PIC X(5).
                  05  Y01-CH-KENSUU84.
                    06  Y01-CH-KENSUU84-STYLE   PIC X(20).
                    06  Y01-KENSUU84            PIC X(5).
                  05  Y01-CH-KENSUU85.
                    06  Y01-CH-KENSUU85-STYLE   PIC X(20).
                    06  Y01-KENSUU85            PIC X(5).
                  05  Y01-CH-KENSUU86.
                    06  Y01-CH-KENSUU86-STYLE   PIC X(20).
                    06  Y01-KENSUU86            PIC X(5).
                  05  Y01-CH-KENSUU87.
                    06  Y01-CH-KENSUU87-STYLE   PIC X(20).
                    06  Y01-KENSUU87            PIC X(5).
                  05  Y01-CH-KENSUU88.
                    06  Y01-CH-KENSUU88-STYLE   PIC X(20).
                    06  Y01-KENSUU88            PIC X(5).
                  05  Y01-CH-KENSUU89.
                    06  Y01-CH-KENSUU89-STYLE   PIC X(20).
                    06  Y01-KENSUU89            PIC X(5).
                  05  Y01-CH-KENSUU90.
                    06  Y01-CH-KENSUU90-STYLE   PIC X(20).
                    06  Y01-KENSUU90            PIC X(5).
                  05  Y01-CH-KENSUU91.
                    06  Y01-CH-KENSUU91-STYLE   PIC X(20).
                    06  Y01-KENSUU91            PIC X(5).
                  05  Y01-CH-KENSUU92.
                    06  Y01-CH-KENSUU92-STYLE   PIC X(20).
                    06  Y01-KENSUU92            PIC X(5).
                  05  Y01-CH-KENSUU93.
                    06  Y01-CH-KENSUU93-STYLE   PIC X(20).
                    06  Y01-KENSUU93            PIC X(5).
                  05  Y01-CH-KENSUU94.
                    06  Y01-CH-KENSUU94-STYLE   PIC X(20).
                    06  Y01-KENSUU94            PIC X(5).
                  05  Y01-CH-KENSUU95.
                    06  Y01-CH-KENSUU95-STYLE   PIC X(20).
                    06  Y01-KENSUU95            PIC X(5).
                  05  Y01-CH-KENSUU96.
                    06  Y01-CH-KENSUU96-STYLE   PIC X(20).
                    06  Y01-KENSUU96            PIC X(5).
                  05  Y01-CH-KENSUU97.
                    06  Y01-CH-KENSUU97-STYLE   PIC X(20).
                    06  Y01-KENSUU97            PIC X(5).
                  05  Y01-CH-KENSUU98.
                    06  Y01-CH-KENSUU98-STYLE   PIC X(20).
                    06  Y01-KENSUU98            PIC X(5).
              03  Y01-SEX.
                04  Y01-VALUE   PIC X(2).
              03  Y01-BARTHDAY.
                04  Y01-VALUE   PIC X(9).
              03  Y01-TAIYYMM.
                04  Y01-VALUE   PIC X(12).
              03  Y01-TAIYMD.
                04  Y01-VALUE   PIC X(16).
              03  Y01-TAIKENSUU.
                04  Y01-VALUE   PIC X(6).
              03  Y01-CH-DAY1.
                04  Y01-CH-DAY1-STYLE   PIC X(20).
                04  Y01-DAY1            PIC X(5).
              03  Y01-CH-DAY2.
                04  Y01-CH-DAY2-STYLE   PIC X(20).
                04  Y01-DAY2            PIC X(5).
              03  Y01-CH-DAY3.
                04  Y01-CH-DAY3-STYLE   PIC X(20).
                04  Y01-DAY3            PIC X(5).
              03  Y01-CH-DAY4.
                04  Y01-CH-DAY4-STYLE   PIC X(20).
                04  Y01-DAY4            PIC X(5).
              03  Y01-CH-DAY5.
                04  Y01-CH-DAY5-STYLE   PIC X(20).
                04  Y01-DAY5            PIC X(5).
              03  Y01-CH-DAY6.
                04  Y01-CH-DAY6-STYLE   PIC X(20).
                04  Y01-DAY6            PIC X(5).
              03  Y01-CH-DAY7.
                04  Y01-CH-DAY7-STYLE   PIC X(20).
                04  Y01-DAY7            PIC X(5).
              03  Y01-YYKYMD.
                04  Y01-VALUE   PIC X(10).
              03  Y01-YYKTIME.
                04  Y01-VALUE   PIC X(5).
              03  Y01-DRNAME-COMBO.
                04  Y01-DRNAME.
                  05  Y01-VALUE   PIC X(29).
                04  Y01-DRNAME-COUNT   PIC S9(9)   BINARY.
                04  Y01-DRNAME-LST     PIC X(29)   OCCURS  99 TIMES.
              03  Y01-SRYNAIYO-COMBO.
                04  Y01-SRYNAIYO.
                  05  Y01-VALUE   PIC X(27).
                04  Y01-SRYNAIYO-COUNT   PIC S9(9)   BINARY.
                04  Y01-SRYNAIYO-LST    PIC X(27)   OCCURS  99 TIMES.
              03  Y01-S-SROLLEDWINDOW3.
                04  Y01-PTLIST.
                  05  Y01-PTLIST-COUNT   PIC S9(9)   BINARY.
                  05  Y01-PTLIST-ROW     PIC S9(9)   BINARY.
                  05  Y01-PTLIST-ROWATTR  PIC S9(9)   BINARY.
                  05  Y01-ITEM        OCCURS  200 TIMES.
                    06  Y01-TBANGO       PIC X(3).
                    06  Y01-TYYKTIME     PIC X(5).
                    06  Y01-TNAME        PIC X(20).
                    06  Y01-TKAMEI       PIC X(4).
                    06  Y01-TRAIINZUMI   PIC X(4).
                  05  Y01-PTLIST-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y01-SRYKA-COMB.
                04  Y01-SRYKA.
                  05  Y01-VALUE   PIC X(23).
                04  Y01-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  Y01-SRYKA-LST    PIC X(23)   OCCURS  99 TIMES.
              03  Y01-KAKREN-COMB.
                04  Y01-KAKREN.
                  05  Y01-VALUE   PIC X(23).
                04  Y01-KAKREN-COUNT   PIC S9(9)   BINARY.
                04  Y01-KAKREN-LST    PIC X(23)   OCCURS  99 TIMES.
              03  Y01-YYKKBN-COMB.
                04  Y01-YYKKBN.
                  05  Y01-VALUE   PIC X(33).
                04  Y01-YYKKBN-COUNT   PIC S9(9)   BINARY.
                04  Y01-YYKKBN-LST    PIC X(33)   OCCURS  100 TIMES.
              03  Y01-COMMENT1.
                04  Y01-VALUE   PIC X(80).
              03  Y01-YYKHYO-COMB.
                04  Y01-YYKHYO.
                  05  Y01-VALUE   PIC X(24).
                04  Y01-YYKHYO-COUNT   PIC S9(9)   BINARY.
                04  Y01-YYKHYO-ITEM
                        PIC X(24)   OCCURS  5 TIMES.

