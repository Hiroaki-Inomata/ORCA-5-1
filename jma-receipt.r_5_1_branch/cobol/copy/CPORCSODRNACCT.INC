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
      *    ��������������׺���  COPY (CPORCSODRNACCT.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����     ����
      ************************************************************
       01  SODRNACCT-AREA.
      *    ��ư��ͳ
           03  SODRNACCT-ORDERID       PIC X(04).
      *    ��������ʬ��01:���̡�02:����������
           03  SODRNACCT-NYUKBN        PIC X(02).
      *    �������û�
           03  SODRNACCT-KSN-G.
               05  SODRNACCT-KSN-MAX   PIC 9(03).
               05  SODRNACCT-KSN-SRYCD PIC X(09)   OCCURS  3.
      *    ���񡦿���������
           03  SODRNACCT-STARTYMD      PIC X(08).
      *    ����λ��
           03  SODRNACCT-ENDYMD        PIC X(08).
      *    ���񡦿�������
           03  SODRNACCT-SKJTIME       PIC X(02).
      *    ����
           03  SODRNACCT-DAY-G.
               05  SODRNACCT-DAY       PIC 9(1)
                                       OCCURS  31.
      *    ������³��ʬ
           03  SODRNACCT-SAME-AFTER    PIC X(1).
      *
      *    ���Ųʡ�ž�����
           03  SODRNACCT-SRYKA-G.
               05  SODRNACCT-SRYKA-CNT PIC 9(05).
               05  SODRNACCT-OCC       OCCURS  50.
                   07  SODRNACCT-SRYKA PIC X(02).
      *
           03  SODRNACCT-OPID          PIC X(16).
           03  SODRNACCT-UPYMD         PIC X(08).
           03  SODRNACCT-UPHMS         PIC X(06).
      *
      *    �ֵ���
           03  SODRNACCT-RC            PIC S9(02).
