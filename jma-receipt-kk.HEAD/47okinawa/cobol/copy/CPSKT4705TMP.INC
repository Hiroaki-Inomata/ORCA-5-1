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
      ********************************************************************
      *    ������������ɽ �ʲ��츩��     COPY (CPSKT4705TMP.INC.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  17/10/05    ���          ��������
      ********************************************************************
       01  SKT4705TMP-REC.
           03  SKT4705TMP-KEY.
               05  SKT4705TMP-RECEKA                PIC  X(02).
           03  SKT4705TMP-HKN-AREA      OCCURS  2.
               05  SKT4705TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4705TMP-KENSU             PIC  9(05).
               05  SKT4705TMP-SKENSU                PIC  9(06).
           03  SKT4705TMP-GKENSU                    PIC  9(06).
           03  SKT4705TMP-KOH-AREA  OCCURS  20.
               05  SKT4705TMP-KOHNUM                PIC  X(03).
               05  SKT4705TMP-KOHNAME               PIC  X(10).
               05  SKT4705TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4705TMP-KKENSU            PIC  9(05).
           03  SKT4705TMP-GKKENSU                   PIC  9(06).