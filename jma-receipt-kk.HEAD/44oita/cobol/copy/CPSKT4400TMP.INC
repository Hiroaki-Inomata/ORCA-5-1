      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *    ����������ɼ     COPY (CPSKT4400TMP.INC)             
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SKT4400TMP-REC.
           03  SKT4400TMP-KEY.
               05  SKT4400TMP-PREFKBN    PIC X(01).
               05  SKT4400TMP-KOUKI      PIC X(01).
           03  SKT4400TMP-HKN-TBL OCCURS 2.
               05  SKT4400TMP-NYUGAI-TBL OCCURS 2.
                   07  SKT4400TMP-KENSU  PIC 9(06).
                   07  SKT4400TMP-TENSU  PIC 9(10).
               05  SKT4400TMP-GKENSU     PIC 9(05).
           03  SKT4400TMP-SAI-KENSU      PIC 9(06) OCCURS 2.
