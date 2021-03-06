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
      *    �̻����������å�����      COPY (CPORCSTUSANCHK.INC)   *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTUSANCHKAREA.
      *  ���ŵ��إ�����
           03  STUSANCHK-HOSPNUM           PIC 9(2).
      *  ���ԣɣ�
           03  STUSANCHK-PTID              PIC  9(10).
      *    ������
           03  STUSANCHK-NYUINYMD          PIC X(08).
      *    �����
           03  STUSANCHK-KJNYMD            PIC X(08).
      *    �����ֹ�
           03  STUSANCHK-RRKNUM            PIC 9(03).
      *  �����ΰ�
           03  STUSANCHK-OUT-AREA.
      *        ���������̻�����
               05  STUSANCHK-TSUSAN        PIC 9(08).
      *        �������ʾ���ã������
      *          0...������Ͽ�Ǻ���������������ã
      *          1...������������ã
      *          9...�����Բ�
               05  STUSANCHK-DAY-91-STS    PIC 9(1).
      *        �������ʾ���ã��
               05  STUSANCHK-DAY-91        PIC X(08).
      *        ���������ʾ���ã������
      *          0...������Ͽ�Ǻ���������������ã
      *          1...¾����������ã
      *          2...������������ã
      *          9...�����Բ�
               05  STUSANCHK-DAY-180-STS   PIC 9(1).
      *        ���������ʾ���ã��
               05  STUSANCHK-DAY-180       PIC X(08).
      *  �꥿���󥳡���
           03  STUSANCHK-RC                PIC 9(02).
