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
      *    �������������������������֡�COPY (CPORCSS011.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SS011-AREA.
           03  SS011-PTID                  PIC 9(10).
      *    ������������������ե饰��SS011-LIFE,SS011-DAY������)
      *    0:���֤����ꤹ�� / 1:�ƽи������ꤹ��
           03  SS011-DAYFLG                PIC 9(01).
      *    ���Ŷ�ʬ�����ģ���������ե饰��SS011-ADLTEN,MEDKBN������)
      *    0:���֤����ꤹ�� / 1:�ƽи������ꤹ��
           03  SS011-MEDADLFLG             PIC 9(01).
      *    ���������������ե饰
      *    0:�������ʤ���1:��������
           03  SS011-NYUUPDFLG             PIC 9(01).
           03  SS011-SRYYM                 PIC X(06).
           03  SS011-DAY-G.
               05  SS011-DAY-OCC           OCCURS    31.
      *    �����������μ���(CPRYOYOCD.INC���ȡ�
                   07  SS011-KHN           PIC 9(02).
      *    ������פ����������Ƚ�Ǥ�����ʬ
      *    1:����2:�¡�3:��...9:�ɡ�����������������1���ֵѡ�
                   07  SS011-DAY           PIC 9(02).
      *    ���Ŷ�ʬ�����ģ��������Ƚ�Ǥ�����ʬ�����������������������
                   07  SS011-DAY2          PIC 9(02).
      *    ���Ŷ�ʬ�����ģ��������Ƚ�Ǥ�����ʬ������������������ޤ��
                   07  SS011-DAY3          PIC 9(02).
                   07  SS011-ADLTEN        PIC 9(03).
                   07  SS011-MEDKBN        PIC 9(01).
      *    ���Ŷ�ʬ�����ģ��������Ƚ�Ǥ���������������
                   07  SS011-SRYCD         PIC X(09).
           03  SS011-RC                    PIC S9(9) BINARY.