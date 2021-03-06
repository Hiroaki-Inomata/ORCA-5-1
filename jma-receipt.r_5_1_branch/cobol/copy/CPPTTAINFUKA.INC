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
      *    ����¾�����ղþ���        COPY (CPPTTAINFUKA.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTTAINFUKA-REC.
      *����
           03  PTTAINFUKA-KEY.
      *���ŵ��أɣ�
               05  PTTAINFUKA-HOSPNUM              PIC 9(2).
      *���ԣɣ�
               05  PTTAINFUKA-PTID                PIC  9(10).
      *���������ֹ����������ǡܣ����ĺ��֡�
               05  PTTAINFUKA-RRKNUM              PIC  9(03).
      *��̾
           03  PTTAINFUKA-BYOMEI                  PIC  X(80)
                                                  OCCURS   5.
      *����������
           03  PTTAINFUKA-KHNRYO                  PIC  X(80).
      *ü���ɣ�
           03  PTTAINFUKA-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTTAINFUKA-OPID                    PIC  X(16).
      *����ǯ����
           03  PTTAINFUKA-CREYMD                  PIC  X(08).
      *����ǯ����
           03  PTTAINFUKA-UPYMD                   PIC  X(08).
      *��������
           03  PTTAINFUKA-UPHMS                   PIC  X(06).
