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
      *    �����������     COPY (CPSKT4005MP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������     ����      ����
      * 01.00.01     ����       09/10/14  ���彣�����뱫�б�
      * 01.00.02     ���       19/01/10  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT4005TMP-REC.
           03  SKT4005TMP-KEY.
               05  SKT4005TMP-GENMEN            PIC  X(01).
               05  SKT4005TMP-SYUTURYOKU        PIC  9(01).
               05  SKT4005TMP-PERFNUM           PIC  X(02).
           03  SKT4005TMP-NYUGAIKBN   OCCURS  2.
               05  SKT4005TMP-KENSU             PIC  9(06).
               05  SKT4005TMP-TENSU             PIC  9(10).
