      ******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  VXe¼        : nqb`
      *  TuVXe¼    :  [
      *  R|[lg¼  : ¢ûàê\
      *  ÇÒ            : 
      *  Maj/Min/Rev  C³Ò  út       àe
      *   01.00.00    åÔ    11/03/29   VKì¬
      ******************************************************************
       01  TA03.
      *    fÃNú
           02  TA03-SRYSTYMD         PIC X(22).
      *    fÃNú
           02  TA03-SRYEDYMD         PIC X(22).
      *    ì¬ú
           02  TA03-CREATEYMD        PIC X(22).
      *    Íß°¼Þ
           02  TA03-PAGE             PIC X(2).
      *    Íß°¼Þ
           02  TA03-GPAGE            PIC X(2).
      *    íÊ
           02  TA03-NYUGAI           PIC X(6).
      *    aº^fÃÈ
           02  TA03-LABEL            PIC X(10).
           02  TA03-MEISAI-TBL       OCCURS  34 TIMES.
      *      AÔ
             03  TA03-RENNUM         PIC X(6).
      *      ³ÒÔ
             03  TA03-PTNUM          PIC X(10).
      *      ³ÒÔ2-1
             03  TA03-PTNUM2-1       PIC X(10).
      *      ³ÒÔ2-2
             03  TA03-PTNUM2-2       PIC X(10).
      *      ¼
             03  TA03-NAME           PIC X(20).
      *      Û¯
             03  TA03-HKN            PIC X(4).
      *      ¿ú
             03  TA03-SEIKYUMD       PIC X(9).
      *      `[Ô
             03  TA03-DENPNUM        PIC X(7).
      *      aº^fÃÈ
             03  TA03-SRYKA-BR       PIC X(8).
      *      Û¯v
             03  TA03-HKN-TOTAL      PIC X(9).
      *      ³ÒS
             03  TA03-KANJAFTN       PIC X(9).
      *      ©ï
             03  TA03-JIHI           PIC X(9).
      *      ¢ûz
             03  TA03-MISYU          PIC X(9).
      *      üàz
             03  TA03-NYUKIN         PIC X(9).
      *      üàú
             03  TA03-NYUKINMD       PIC X(9).
      *      ÅIófú
             03  TA03-JYUSHINYMD     PIC X(9).
      *      dbÔ
             03  TA03-TEL            PIC X(15).
      *    v
           02  TA03-GOKEI-TBL.
      *      
             03  TA03-GKENSU         PIC X(6).
      *      Û¯v
             03  TA03-GHKN-TOTAL     PIC X(10).
      *      ³ÒS
             03  TA03-GKANJAFTN      PIC X(10).
      *      ©ï
             03  TA03-GJIHI          PIC X(10).
      *      ¢ûz
             03  TA03-GMISYU         PIC X(10).
      *      üàz
             03  TA03-GNYUKIN        PIC X(10).
