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
      *                     COPY (CPSKT2515TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/09/16  ��ƻ�ܸ��ֹ��ɲ�
      *  01.00.02    ���         18/12/12  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT2515TMP-REC.
           03  SKT2515TMP-KEY.
               05  SKT2515TMP-KBN                   PIC  9(01).
               05  SKT2515TMP-PREFNUM               PIC  X(02).
               05  SKT2515TMP-HKNJANUM              PIC  X(06).
               05  SKT2515TMP-KYURATE               PIC  9(03).
               05  SKT2515TMP-IDX                   PIC  9(02).
           03  SKT2515TMP-SEIKYU.
               07  SKT2515TMP-KENSU                 PIC  9(06).
