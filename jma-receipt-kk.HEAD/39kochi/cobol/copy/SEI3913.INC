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
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/02/22  ¾ˡˡ���ֹ��¾ˡ��ô���ɲ�
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *ˡ���ֹ�
               05  MF100-HBTNUM          PIC  X(02).
      *�ǡ������ꥢ
           03  MF100-DATA-AREA.
               05    MF100-NYUGAI-TBL  OCCURS 2.
      *���
                   07  MF100-KENSU       PIC  9(04).
      *����
                   07  MF100-TENSU       PIC  9(07).
      *����ʬ����
                   07  MF100-KOHTENSU    PIC  9(07).
      *���ް�����ô��
                   07  MF100-YKZFTN      PIC  9(07).
      *Ϸ�Ͱ�����ô��
                   07  MF100-ITBFTN      PIC  9(07).
      *�������
               05  MF100-SKENSU          PIC  9(04).
      *�������
               05  MF100-SKINGK          PIC  9(07).
      *����ɸ����ô��
               05  MF100-SFTNGK          PIC  9(07).
      *¾ˡˡ���ֹ�
           03  MF100-TAHOUHBT            PIC  X(02).
      *¾ˡ��ô��
           03  MF100-TAHOUFTN            PIC  9(07).
