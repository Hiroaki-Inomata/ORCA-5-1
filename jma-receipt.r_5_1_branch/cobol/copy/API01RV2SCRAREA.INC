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
      *    api01rv2　（ＡＰＩ情報取得）
      ******************************************************************
      *ORAPI012R1V2
           COPY    "CPAPIPTAV2.INC".
      *ORAPI011R1V2
           COPY    "CPAPILSTV2.INC".
      *ORAPI014R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST1-//.
      *ORAPI012R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST2-//.
      *ORAPI012R3V2
           COPY    "CPAPILSTV2.INC"  REPLACING
                                     //APILST-// BY //APILST3-//.
      *ORAPI012R4V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST4-//.
      *ORAPI101R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST5-//.
      *ORAPI021R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST6-//.
      *ORAPI022R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST7-//.
      *ORAPI014R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST8-//.
      *ORAPI023R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST9-//.
      *ORAPI021R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST10-//.
      *ORAPI031RC1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST11-//.
      *ORAPI031RC2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST12-//.
      *ORAPI021R3V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST13-//.
      *ORAPI032R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST14-//.
      *ORAPI105R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST15-//.
      *ORAPI032R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST16-//.
      *ORAPI031R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST17-//.
      *ORAPI034R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST18-//.
      *ORAPI023R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST19-//.
      *ORAPI000R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST20-//.
      *ORAPI012R5V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST21-//.
      *ORAPI042R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST22-//.
      *ORAPICLAIM1RV2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST23-//.
      *ORAPICLAIM2RV2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST24-//.
      *ORAPI001R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST25-//.
      *ORAPI021R4V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST26-//.
      *ORAPI052R4V2
      **   COPY    "CPAPILSTV2.INC"    REPLACING
      **                               //APILST-// BY //APILST27-//.
      *ORAPIRELR1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST28-//.
      *ORAPIRELR2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST29-//.
      *ORAPI000R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST30-//.
      *ORAPI012R6V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST31-//.
      *ORAPI001R2V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST32-//.
      *ORAPI001R3V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST33-//.
      *ORAPI025R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST34-//.
      *ORAPI101R1V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST35-//.
      *ORAPI031RC3V2
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                                //APILST-// BY //APILST36-//.
      *ORAPI000R3V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST37-//.
      *ORAPI000R4V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST38-//.
      *ORAPI001R4V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST39-//.
      *ORAPI001R5V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST40-//.
      *ORAPI001R6V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST41-//.
      *ORAPI001R7V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST42-//.
      *ORAPI012R7V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST43-//.
      *ORAPI001R8V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST44-//.
      *ORAPI001R9V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST45-//.
      *ORAPI001R10V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST46-//.
      *ORAPI001R11V2
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //APILST47-//.
